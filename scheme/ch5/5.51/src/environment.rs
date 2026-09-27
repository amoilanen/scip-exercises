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
mod tests;
