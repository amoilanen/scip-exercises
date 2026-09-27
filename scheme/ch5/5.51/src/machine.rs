//! Values and the register machine's list-structured memory, with a
//! stop-and-copy garbage collector (section 5.3).

use crate::eval::Label;
use Value::*;

pub const MEMORY_SIZE: usize = 1 << 18;
const STACK_SIZE: usize = 100_000;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub enum Value {
    #[default]
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Symbol(&'static str),
    Str(&'static str),
    Primitive(&'static str),
    Label(Label),
    Unspecified,
    Pair(usize),
    /// A cell holding a lambda expression and its environment.
    Procedure(usize),
    /// Left by the collector in a moved cell, pointing to its new place.
    BrokenHeart(usize),
}

pub struct Error(pub String);

pub type Result<T> = std::result::Result<T, Error>;

impl From<&str> for Error {
    fn from(message: &str) -> Error {
        Error(message.to_owned())
    }
}

/// Symbol names and strings live until the interpreter exits.
pub fn permanent(text: &str) -> &'static str {
    Box::leak(text.into())
}

#[derive(Default)]
pub struct Machine {
    memory: Vec<(Value, Value)>,
    stack: Vec<Value>,
    pub exp: Value,
    pub env: Value,
    pub val: Value,
    pub cont: Value,
    pub proc: Value,
    pub argl: Value,
    pub unev: Value,
    pub global_env: Value,
}

impl Machine {
    pub fn new() -> Machine {
        let mut machine = Machine::default();
        machine.global_env = machine.setup_environment();
        machine
    }

    pub fn reset(&mut self) {
        *self = Machine {
            memory: std::mem::take(&mut self.memory),
            global_env: self.global_env,
            ..Machine::default()
        };
    }

    pub fn error(&self, message: &str, irritants: &[Value]) -> Error {
        let mut text = message.to_owned();
        for &irritant in irritants {
            text += " ";
            text += &self.show(irritant, true);
        }
        Error(text)
    }

    pub fn cons(&mut self, car: Value, cdr: Value) -> Value {
        self.memory.push((car, cdr));
        Pair(self.memory.len() - 1)
    }

    pub fn make_procedure(&mut self, lambda: Value, env: Value) -> Value {
        self.memory.push((lambda, env));
        Procedure(self.memory.len() - 1)
    }

    /// The lambda expression and the environment of a procedure.
    pub fn procedure_parts(&self, index: usize) -> (Value, Value) {
        self.memory[index]
    }

    fn pair_index(&self, v: Value, operation: &str) -> Result<usize> {
        match v {
            Pair(i) => Ok(i),
            _ => Err(self.error(&format!("The object passed to {operation} is not a pair:"), &[v])),
        }
    }

    pub fn car(&self, pair: Value) -> Result<Value> {
        Ok(self.memory[self.pair_index(pair, "car")?].0)
    }

    pub fn cdr(&self, pair: Value) -> Result<Value> {
        Ok(self.memory[self.pair_index(pair, "cdr")?].1)
    }

    pub fn set_car(&mut self, pair: Value, value: Value) -> Result<()> {
        let i = self.pair_index(pair, "set-car!")?;
        self.memory[i].0 = value;
        Ok(())
    }

    pub fn set_cdr(&mut self, pair: Value, value: Value) -> Result<()> {
        let i = self.pair_index(pair, "set-cdr!")?;
        self.memory[i].1 = value;
        Ok(())
    }

    /// cadr, cddr, caddr and the like: cxr("cadr", x) is (car (cdr x)).
    pub fn cxr(&self, name: &str, v: Value) -> Result<Value> {
        let path = &name[1..name.len() - 1];
        path.bytes().rev().try_fold(
            v,
            |v, step| {
                if step == b'a' {
                    self.car(v)
                } else {
                    self.cdr(v)
                }
            },
        )
    }

    pub fn list(&mut self, items: &[Value], tail: Value) -> Value {
        items.iter().rev().fold(tail, |list, &item| self.cons(item, list))
    }

    /// The items of a possibly improper list and what ends it.
    pub fn items(&self, mut list: Value) -> (Vec<Value>, Value) {
        let mut items = Vec::new();
        while let Pair(i) = list {
            items.push(self.memory[i].0);
            list = self.memory[i].1;
        }
        (items, list)
    }

    pub fn to_vec(&self, list: Value) -> Result<Vec<Value>> {
        match self.items(list) {
            (items, Nil) => Ok(items),
            _ => Err(self.error("The object is not a list:", &[list])),
        }
    }

    pub fn save(&mut self, value: Value) -> Result<()> {
        if self.stack.len() == STACK_SIZE {
            return Err("Aborting!: maximum recursion depth exceeded".into());
        }
        self.stack.push(value);
        Ok(())
    }

    pub fn restore(&mut self) -> Value {
        self.stack.pop().expect("restore without save")
    }

    pub fn memory_used(&self) -> usize {
        self.memory.len()
    }

    /// Must run only when every live value is in a register, on the stack or
    /// in the global environment.
    pub fn collect_garbage(&mut self) -> Result<()> {
        let mut old = std::mem::take(&mut self.memory);
        let new = &mut self.memory;
        let registers = [
            &mut self.exp,
            &mut self.env,
            &mut self.val,
            &mut self.cont,
            &mut self.proc,
            &mut self.argl,
            &mut self.unev,
            &mut self.global_env,
        ];
        for root in registers.into_iter().chain(&mut self.stack) {
            *root = relocate(*root, &mut old, new);
        }
        let mut scan = 0;
        while scan < new.len() {
            let (car, cdr) = new[scan];
            new[scan] = (relocate(car, &mut old, new), relocate(cdr, &mut old, new));
            scan += 1;
        }
        if new.len() < MEMORY_SIZE {
            Ok(())
        } else {
            Err("Aborting!: out of memory".into())
        }
    }
}

fn relocate(v: Value, old: &mut [(Value, Value)], new: &mut Vec<(Value, Value)>) -> Value {
    let mut move_cell = |i: usize| {
        if let BrokenHeart(moved) = old[i].0 {
            return moved;
        }
        new.push(old[i]);
        old[i].0 = BrokenHeart(new.len() - 1);
        new.len() - 1
    };
    match v {
        Pair(i) => Pair(move_cell(i)),
        Procedure(i) => Procedure(move_cell(i)),
        _ => v,
    }
}
