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

#[derive(Debug)]
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

#[cfg(test)]
mod tests {
    use super::*;

    fn numbers(machine: &mut Machine, numbers: &[i64], tail: Value) -> Value {
        let items: Vec<Value> = numbers.iter().map(|&n| Int(n)).collect();
        machine.list(&items, tail)
    }

    #[test]
    fn car_and_cdr_take_a_pair_apart() {
        let mut machine = Machine::new();
        let pair = machine.cons(Int(1), Int(2));
        assert_eq!(machine.car(pair).unwrap(), Int(1));
        assert_eq!(machine.cdr(pair).unwrap(), Int(2));
    }

    #[test]
    fn set_car_and_set_cdr_change_a_pair() {
        let mut machine = Machine::new();
        let pair = machine.cons(Int(1), Int(2));
        machine.set_car(pair, Int(3)).unwrap();
        machine.set_cdr(pair, Nil).unwrap();
        assert_eq!(machine.show(pair, true), "(3)");
    }

    #[test]
    fn pair_operations_reject_other_objects() {
        let mut machine = Machine::new();
        assert_eq!(machine.car(Nil).unwrap_err().0, "The object passed to car is not a pair: ()");
        assert_eq!(machine.cdr(Int(5)).unwrap_err().0, "The object passed to cdr is not a pair: 5");
        assert_eq!(
            machine.set_car(Symbol("x"), Nil).unwrap_err().0,
            "The object passed to set-car! is not a pair: x"
        );
    }

    #[test]
    fn cxr_applies_its_car_and_cdr_steps_from_right_to_left() {
        let mut machine = Machine::new();
        let inner = numbers(&mut machine, &[2, 3], Nil);
        let list = machine.list(&[Int(1), inner, Int(4)], Nil);
        assert_eq!(machine.show(machine.cxr("cadr", list).unwrap(), true), "(2 3)");
        assert_eq!(machine.cxr("caadr", list).unwrap(), Int(2));
        assert_eq!(machine.show(machine.cxr("cddr", list).unwrap(), true), "(4)");
        assert_eq!(
            machine.cxr("cadddr", list).unwrap_err().0,
            "The object passed to car is not a pair: ()"
        );
    }

    #[test]
    fn lists_convert_to_and_from_items() {
        let mut machine = Machine::new();
        let dotted = numbers(&mut machine, &[1, 2], Int(3));
        assert_eq!(machine.items(dotted), (vec![Int(1), Int(2)], Int(3)));
        assert_eq!(machine.to_vec(dotted).unwrap_err().0, "The object is not a list: (1 2 . 3)");
        let proper = numbers(&mut machine, &[1, 2], Nil);
        assert_eq!(machine.to_vec(proper).unwrap(), vec![Int(1), Int(2)]);
        assert_eq!(machine.to_vec(Nil).unwrap(), vec![]);
    }

    #[test]
    fn the_stack_restores_in_reverse_order_and_is_bounded() {
        let mut machine = Machine::new();
        machine.save(Int(1)).unwrap();
        machine.save(Int(2)).unwrap();
        assert_eq!(machine.restore(), Int(2));
        assert_eq!(machine.restore(), Int(1));
        for _ in 0..STACK_SIZE {
            machine.save(Nil).unwrap();
        }
        assert_eq!(machine.save(Nil).unwrap_err().0, "Aborting!: maximum recursion depth exceeded");
    }

    #[test]
    fn garbage_collection_keeps_only_what_the_roots_reach() {
        let mut machine = Machine::new();
        machine.val = numbers(&mut machine, &[1, 2, 3], Nil);
        let list = numbers(&mut machine, &[4, 5], Nil);
        machine.save(list).unwrap();
        let live = machine.memory_used();
        for n in 0..1000 {
            machine.cons(Int(n), Nil);
        }
        machine.collect_garbage().unwrap();
        assert_eq!(machine.memory_used(), live);
        assert_eq!(machine.show(machine.val, true), "(1 2 3)");
        let list = machine.restore();
        assert_eq!(machine.show(list, true), "(4 5)");
        let car = machine.lookup_variable_value(Symbol("car"), machine.global_env).unwrap();
        assert_eq!(car, Primitive("car"));
    }

    #[test]
    fn garbage_collection_preserves_sharing_and_cycles() {
        let mut machine = Machine::new();
        let shared = machine.cons(Int(1), Nil);
        machine.val = machine.cons(shared, shared);
        machine.exp = machine.cons(Int(2), Nil);
        machine.set_cdr(machine.exp, machine.exp).unwrap();
        machine.collect_garbage().unwrap();
        assert_eq!(machine.car(machine.val).unwrap(), machine.cdr(machine.val).unwrap());
        assert_eq!(machine.cdr(machine.exp).unwrap(), machine.exp);
        assert_eq!(machine.car(machine.exp).unwrap(), Int(2));
    }

    #[test]
    fn garbage_collection_moves_procedures_with_their_environments() {
        let mut machine = Machine::new();
        let lambda = numbers(&mut machine, &[1], Nil);
        let env = numbers(&mut machine, &[2], Nil);
        machine.proc = machine.make_procedure(lambda, env);
        machine.collect_garbage().unwrap();
        let Procedure(i) = machine.proc else { panic!("not a procedure: {:?}", machine.proc) };
        let (lambda, env) = machine.procedure_parts(i);
        assert_eq!(
            (machine.show(lambda, true), machine.show(env, true)),
            ("(1)".into(), "(2)".into())
        );
    }

    #[test]
    fn garbage_collection_fails_when_live_data_fills_the_memory() {
        let mut machine = Machine::new();
        machine.val = machine.list(&vec![Int(0); MEMORY_SIZE], Nil);
        assert_eq!(machine.collect_garbage().unwrap_err().0, "Aborting!: out of memory");
    }

    #[test]
    fn reset_clears_the_registers_and_the_stack_but_keeps_the_global_environment() {
        let mut machine = Machine::new();
        let global_env = machine.global_env;
        machine.val = Int(1);
        machine.save(Int(2)).unwrap();
        machine.reset();
        assert_eq!(machine.val, Nil);
        assert_eq!(machine.global_env, global_env);
        machine.save(Int(3)).unwrap();
        assert_eq!(machine.restore(), Int(3));
    }
}
