//! Environments as lists of frames, each frame a pair of a list of
//! variables and a list of their values (section 4.1.3).

use crate::error::Result;
use crate::machine::Machine;
use crate::object::Value;

impl Machine {
    fn add_binding_to_frame(&mut self, variable: Value, value: Value, frame: Value) -> Result<()> {
        let frame_slot = self.protect(frame)?;
        let variable_slot = self.protect(variable)?;
        let values = self.cdr(frame)?;
        let values = self.cons(value, values)?;
        let frame = self.protected(frame_slot);
        self.set_cdr(frame, values)?;
        let variables = self.car(frame)?;
        let variables = self.cons(self.protected(variable_slot), variables)?;
        let frame = self.protected(frame_slot);
        self.set_car(frame, variables)?;
        self.unprotect(2);
        Ok(())
    }

    /// For (a b . rest) and (1 2 3 4) makes a frame binding a to 1, b to 2
    /// and rest to (3 4).
    fn make_frame_with_rest(&mut self, parameters: Value, arguments: Value) -> Result<Value> {
        let frame = self.cons(Value::EmptyList, Value::EmptyList)?;
        let frame = self.protect(frame)?;
        let parameters = self.protect(parameters)?;
        let arguments = self.protect(arguments)?;
        while self.protected(parameters).is_pair() {
            let variable = self.car(self.protected(parameters))?;
            let value = self.car(self.protected(arguments))?;
            self.add_binding_to_frame(variable, value, self.protected(frame))?;
            let rest = self.cdr(self.protected(parameters))?;
            self.set_protected(parameters, rest);
            let rest = self.cdr(self.protected(arguments))?;
            self.set_protected(arguments, rest);
        }
        let (rest_parameter, rest_arguments) =
            (self.protected(parameters), self.protected(arguments));
        self.add_binding_to_frame(rest_parameter, rest_arguments, self.protected(frame))?;
        let frame = self.protected(frame);
        self.unprotect(3);
        Ok(frame)
    }

    /// The parameters may end in a dotted rest parameter, as in
    /// (a b . rest).
    pub fn extend_environment(
        &mut self,
        parameters: Value,
        arguments: Value,
        base: Value,
    ) -> Result<Value> {
        let (mut p, mut a) = (parameters, arguments);
        while p.is_pair() && a.is_pair() {
            p = self.cell_cdr(p);
            a = self.cell_cdr(a);
        }
        if p.is_pair() {
            return Err(self.error("Too few arguments supplied for", parameters));
        }
        if p.is_null() && !a.is_null() {
            return Err(self.error("Too many arguments supplied for", parameters));
        }
        if !p.is_null() && !p.is_symbol() {
            return Err(self.error("Bad parameter list", parameters));
        }

        let base = self.protect(base)?;
        let frame = if p.is_null() {
            self.cons(parameters, arguments)?
        } else {
            self.make_frame_with_rest(parameters, arguments)?
        };
        let base_env = self.protected(base);
        self.unprotect(1);
        self.cons(frame, base_env)
    }

    /// Returns the pair of the frame's value list that holds the variable's
    /// value, or the empty list if the frame has no binding for it.
    fn find_in_frame(&self, variable: Value, frame: Value) -> Result<Value> {
        let mut variables = self.car(frame)?;
        let mut values = self.cdr(frame)?;
        while variables.is_pair() {
            if self.cell_car(variables) == variable {
                return Ok(values);
            }
            variables = self.cell_cdr(variables);
            values = self.cdr(values)?;
        }
        Ok(Value::EmptyList)
    }

    fn find_binding(&self, variable: Value, env: Value, message: &str) -> Result<Value> {
        let mut env = env;
        while env.is_pair() {
            let cell = self.find_in_frame(variable, self.cell_car(env))?;
            if cell.is_pair() {
                return Ok(cell);
            }
            env = self.cell_cdr(env);
        }
        Err(self.error(message, variable))
    }

    pub fn lookup_variable_value(&self, variable: Value, env: Value) -> Result<Value> {
        self.car(self.find_binding(variable, env, "Unbound variable")?)
    }

    pub fn set_variable_value(&mut self, variable: Value, value: Value, env: Value) -> Result<()> {
        let cell = self.find_binding(variable, env, "Unbound variable -- SET!")?;
        self.set_car(cell, value)
    }

    pub fn define_variable(&mut self, variable: Value, value: Value, env: Value) -> Result<()> {
        let frame = self.car(env)?;
        let cell = self.find_in_frame(variable, frame)?;
        if cell.is_pair() {
            self.set_car(cell, value)
        } else {
            self.add_binding_to_frame(variable, value, frame)
        }
    }
}
