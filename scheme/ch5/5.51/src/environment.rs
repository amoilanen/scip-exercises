//! Environments as lists of frames, each frame a pair of a list of
//! variables and a list of their values (section 4.1.3).

use crate::machine::{Machine, Result, Value, Value::*};

impl Machine {
    pub fn make_frame(&mut self, variables: &[Value], values: &[Value]) -> Value {
        let variables = self.list(variables, Nil);
        let values = self.list(values, Nil);
        self.cons(variables, values)
    }

    /// The parameters may end in a rest parameter, as in (a b . rest).
    pub fn extend_environment(
        &mut self,
        parameters: Value,
        arguments: &[Value],
        base: Value,
    ) -> Result<Value> {
        let (mut variables, rest) = self.items(parameters);
        let count = variables.len();
        if arguments.len() < count {
            return Err(self.error("Too few arguments supplied for", &[parameters]));
        }
        let mut values = arguments[..count].to_vec();
        match rest {
            Nil if arguments.len() > count => {
                return Err(self.error("Too many arguments supplied for", &[parameters]));
            }
            Nil => {}
            Symbol(_) => {
                variables.push(rest);
                values.push(self.list(&arguments[count..], Nil));
            }
            _ => return Err(self.error("Bad parameter list", &[parameters])),
        }
        let frame = self.make_frame(&variables, &values);
        Ok(self.cons(frame, base))
    }

    /// The pair whose car holds the value of the variable in the frame.
    fn binding_in_frame(&self, variable: Value, frame: Value) -> Result<Option<Value>> {
        let (mut variables, mut values) = (self.car(frame)?, self.cdr(frame)?);
        while let Pair(_) = variables {
            if self.car(variables)? == variable {
                return Ok(Some(values));
            }
            variables = self.cdr(variables)?;
            values = self.cdr(values)?;
        }
        Ok(None)
    }

    fn binding(&self, variable: Value, mut env: Value) -> Result<Option<Value>> {
        while let Pair(_) = env {
            if let Some(binding) = self.binding_in_frame(variable, self.car(env)?)? {
                return Ok(Some(binding));
            }
            env = self.cdr(env)?;
        }
        Ok(None)
    }

    pub fn lookup_variable_value(&self, variable: Value, env: Value) -> Result<Value> {
        match self.binding(variable, env)? {
            Some(binding) => self.car(binding),
            None => Err(self.error("Unbound variable", &[variable])),
        }
    }

    pub fn set_variable_value(&mut self, variable: Value, value: Value, env: Value) -> Result<()> {
        match self.binding(variable, env)? {
            Some(binding) => self.set_car(binding, value),
            None => Err(self.error("Unbound variable -- SET!", &[variable])),
        }
    }

    pub fn define_variable(&mut self, variable: Value, value: Value, env: Value) -> Result<()> {
        let frame = self.car(env)?;
        if let Some(binding) = self.binding_in_frame(variable, frame)? {
            return self.set_car(binding, value);
        }
        let variables = self.cons(variable, self.car(frame)?);
        let values = self.cons(value, self.cdr(frame)?);
        self.set_car(frame, variables)?;
        self.set_cdr(frame, values)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn symbols(machine: &mut Machine, names: &[&'static str], tail: Value) -> Value {
        let items: Vec<Value> = names.iter().map(|&name| Symbol(name)).collect();
        machine.list(&items, tail)
    }

    fn lookup(machine: &Machine, name: &'static str, env: Value) -> String {
        match machine.lookup_variable_value(Symbol(name), env) {
            Ok(value) => machine.show(value, true),
            Err(error) => error.0,
        }
    }

    #[test]
    fn extending_binds_the_parameters_to_the_arguments() {
        let mut machine = Machine::new();
        let parameters = symbols(&mut machine, &["x", "y"], Nil);
        let env = machine.extend_environment(parameters, &[Int(1), Int(2)], Nil).unwrap();
        assert_eq!(lookup(&machine, "x", env), "1");
        assert_eq!(lookup(&machine, "y", env), "2");
    }

    #[test]
    fn a_rest_parameter_takes_the_remaining_arguments() {
        let mut machine = Machine::new();
        let parameters = symbols(&mut machine, &["a"], Symbol("rest"));
        let env = machine.extend_environment(parameters, &[Int(1), Int(2), Int(3)], Nil).unwrap();
        assert_eq!(lookup(&machine, "a", env), "1");
        assert_eq!(lookup(&machine, "rest", env), "(2 3)");

        let env = machine.extend_environment(Symbol("args"), &[], Nil).unwrap();
        assert_eq!(lookup(&machine, "args", env), "()");
    }

    #[test]
    fn the_arguments_must_match_the_parameters() {
        let mut machine = Machine::new();
        let parameters = symbols(&mut machine, &["x", "y"], Nil);
        let error = |machine: &mut Machine, parameters, arguments: &[Value]| {
            machine.extend_environment(parameters, arguments, Nil).unwrap_err().0
        };
        assert_eq!(
            error(&mut machine, parameters, &[Int(1)]),
            "Too few arguments supplied for (x y)"
        );
        assert_eq!(
            error(&mut machine, parameters, &[Int(1), Int(2), Int(3)]),
            "Too many arguments supplied for (x y)"
        );
        let parameters = symbols(&mut machine, &["x"], Int(5));
        assert_eq!(error(&mut machine, parameters, &[Int(1)]), "Bad parameter list (x . 5)");
    }

    #[test]
    fn lookup_finds_the_innermost_binding() {
        let mut machine = Machine::new();
        let outer = machine.extend_environment(Nil, &[], Nil).unwrap();
        machine.define_variable(Symbol("x"), Int(1), outer).unwrap();
        machine.define_variable(Symbol("y"), Int(2), outer).unwrap();
        let parameters = symbols(&mut machine, &["x"], Nil);
        let inner = machine.extend_environment(parameters, &[Int(3)], outer).unwrap();
        assert_eq!(lookup(&machine, "x", inner), "3");
        assert_eq!(lookup(&machine, "y", inner), "2");
        assert_eq!(lookup(&machine, "x", outer), "1");
        assert_eq!(lookup(&machine, "z", inner), "Unbound variable z");
    }

    #[test]
    fn define_binds_in_the_first_frame_only() {
        let mut machine = Machine::new();
        let outer = machine.extend_environment(Nil, &[], Nil).unwrap();
        let inner = machine.extend_environment(Nil, &[], outer).unwrap();
        machine.define_variable(Symbol("x"), Int(1), inner).unwrap();
        machine.define_variable(Symbol("x"), Int(2), inner).unwrap();
        assert_eq!(lookup(&machine, "x", inner), "2");
        assert_eq!(lookup(&machine, "x", outer), "Unbound variable x");
    }

    #[test]
    fn set_changes_the_innermost_binding() {
        let mut machine = Machine::new();
        let outer = machine.extend_environment(Nil, &[], Nil).unwrap();
        machine.define_variable(Symbol("x"), Int(1), outer).unwrap();
        let inner = machine.extend_environment(Nil, &[], outer).unwrap();
        machine.set_variable_value(Symbol("x"), Int(2), inner).unwrap();
        assert_eq!(lookup(&machine, "x", outer), "2");
        assert_eq!(
            machine.set_variable_value(Symbol("z"), Int(3), inner).unwrap_err().0,
            "Unbound variable -- SET! z"
        );
    }
}
