//! List-structured memory with a stop-and-copy garbage collector
//! (section 5.3), and the stack of the register machine.
//!
//! Any allocation may move every pair. A `Value` held in a local variable
//! across an allocation must therefore be reachable from a root: a
//! register, the global environment, the stack, or a slot made with
//! `Machine::protect`. The arguments of the allocation itself are safe, as
//! the collector relocates them too.

use crate::error::{Error, Result};
use crate::machine::Machine;
use crate::object::Value;

/// The number of cells, pairs and procedures, that the memory holds.
pub const MEMORY_SIZE: usize = 1 << 18;

/// The number of values that the stack holds.
pub const STACK_SIZE: usize = 100_000;

/// The number of values that can be protected at the same time.
const MAX_PROTECTED: usize = 1024;

/// The two halves of every cell, indexed by the pointers, and the new space
/// that the collector copies the live cells into.
pub struct Heap {
    the_cars: Vec<Value>,
    the_cdrs: Vec<Value>,
    new_cars: Vec<Value>,
    new_cdrs: Vec<Value>,
    free: usize,
}

impl Heap {
    pub fn new() -> Heap {
        let cells = || vec![Value::EmptyList; MEMORY_SIZE];
        Heap {
            the_cars: cells(),
            the_cdrs: cells(),
            new_cars: cells(),
            new_cdrs: cells(),
            free: 0,
        }
    }

    fn relocate(&mut self, v: Value) -> Value {
        match v {
            Value::Pair(old) => Value::Pair(self.move_cell(old)),
            Value::Procedure(old) => Value::Procedure(self.move_cell(old)),
            _ => v,
        }
    }

    /// Moves the cell at old into new space, unless it has been moved
    /// already, in which case the broken heart in its car tells where it
    /// went, and returns its new index.
    fn move_cell(&mut self, old: usize) -> usize {
        if let Value::BrokenHeart(new) = self.the_cars[old] {
            return new;
        }
        let new = self.free;
        self.new_cars[new] = self.the_cars[old];
        self.new_cdrs[new] = self.the_cdrs[old];
        self.the_cars[old] = Value::BrokenHeart(new);
        self.free += 1;
        new
    }

    /// Relocates what the cells moved so far point to, which moves more
    /// cells, until every live cell is in new space. Then new space becomes
    /// the working memory.
    fn scan_and_flip(&mut self) {
        let mut scan = 0;
        while scan < self.free {
            let car = self.relocate(self.new_cars[scan]);
            self.new_cars[scan] = car;
            let cdr = self.relocate(self.new_cdrs[scan]);
            self.new_cdrs[scan] = cdr;
            scan += 1;
        }
        std::mem::swap(&mut self.the_cars, &mut self.new_cars);
        std::mem::swap(&mut self.the_cdrs, &mut self.new_cdrs);
    }
}

fn cell_index(cell: Value) -> usize {
    match cell {
        Value::Pair(index) | Value::Procedure(index) => index,
        _ => panic!("not a cell: {cell:?}"),
    }
}

impl Machine {
    fn collect_garbage(&mut self, car: &mut Value, cdr: &mut Value) {
        let heap = &mut self.heap;
        heap.free = 0;
        *car = heap.relocate(*car);
        *cdr = heap.relocate(*cdr);
        for register in self.reg.all_mut() {
            *register = heap.relocate(*register);
        }
        self.global_env = heap.relocate(self.global_env);
        for value in self.stack.iter_mut().chain(self.protected.iter_mut()) {
            *value = heap.relocate(*value);
        }
        heap.scan_and_flip();
    }

    /// Allocates a cell, which pointer, such as `Value::Pair`, makes a value
    /// of.
    pub fn make_cell(
        &mut self,
        pointer: fn(usize) -> Value,
        mut car: Value,
        mut cdr: Value,
    ) -> Result<Value> {
        if self.heap.free == MEMORY_SIZE {
            self.collect_garbage(&mut car, &mut cdr);
            if self.heap.free == MEMORY_SIZE {
                return Err(Error::abort("Aborting!: out of memory"));
            }
        }
        let index = self.heap.free;
        self.heap.the_cars[index] = car;
        self.heap.the_cdrs[index] = cdr;
        self.heap.free += 1;
        Ok(pointer(index))
    }

    pub fn cons(&mut self, car: Value, cdr: Value) -> Result<Value> {
        self.make_cell(Value::Pair, car, cdr)
    }

    /// Unchecked access to the two halves of a pair or procedure.
    pub fn cell_car(&self, cell: Value) -> Value {
        self.heap.the_cars[cell_index(cell)]
    }

    pub fn cell_cdr(&self, cell: Value) -> Value {
        self.heap.the_cdrs[cell_index(cell)]
    }

    fn wrong_type_pair(&self, operation: &str, object: Value) -> Error {
        self.error(
            &format!("The object passed to {operation} is not a pair:"),
            object,
        )
    }

    pub fn car(&self, pair: Value) -> Result<Value> {
        match pair {
            Value::Pair(index) => Ok(self.heap.the_cars[index]),
            _ => Err(self.wrong_type_pair("car", pair)),
        }
    }

    pub fn cdr(&self, pair: Value) -> Result<Value> {
        match pair {
            Value::Pair(index) => Ok(self.heap.the_cdrs[index]),
            _ => Err(self.wrong_type_pair("cdr", pair)),
        }
    }

    pub fn caar(&self, v: Value) -> Result<Value> {
        self.car(self.car(v)?)
    }

    pub fn cadr(&self, v: Value) -> Result<Value> {
        self.car(self.cdr(v)?)
    }

    pub fn cdar(&self, v: Value) -> Result<Value> {
        self.cdr(self.car(v)?)
    }

    pub fn cddr(&self, v: Value) -> Result<Value> {
        self.cdr(self.cdr(v)?)
    }

    pub fn caddr(&self, v: Value) -> Result<Value> {
        self.car(self.cddr(v)?)
    }

    pub fn cdddr(&self, v: Value) -> Result<Value> {
        self.cdr(self.cddr(v)?)
    }

    pub fn set_car(&mut self, pair: Value, value: Value) -> Result<()> {
        match pair {
            Value::Pair(index) => {
                self.heap.the_cars[index] = value;
                Ok(())
            }
            _ => Err(self.wrong_type_pair("set-car!", pair)),
        }
    }

    pub fn set_cdr(&mut self, pair: Value, value: Value) -> Result<()> {
        match pair {
            Value::Pair(index) => {
                self.heap.the_cdrs[index] = value;
                Ok(())
            }
            _ => Err(self.wrong_type_pair("set-cdr!", pair)),
        }
    }

    pub fn save(&mut self, value: Value) -> Result<()> {
        if self.stack.len() == STACK_SIZE {
            return Err(Error::abort("Aborting!: maximum recursion depth exceeded"));
        }
        self.stack.push(value);
        Ok(())
    }

    /// The controller restores only what it has saved.
    pub fn restore(&mut self) -> Value {
        self.stack.pop().expect("restore from an empty stack")
    }

    /// Keeps value safe from the garbage collector until the matching
    /// unprotect, and returns the slot that holds its current location.
    pub fn protect(&mut self, value: Value) -> Result<usize> {
        if self.protected.len() == MAX_PROTECTED {
            return Err(Error::abort("Aborting!: nesting too deep"));
        }
        self.protected.push(value);
        Ok(self.protected.len() - 1)
    }

    pub fn protected(&self, slot: usize) -> Value {
        self.protected[slot]
    }

    pub fn set_protected(&mut self, slot: usize, value: Value) {
        self.protected[slot] = value;
    }

    /// Releases the count slots protected last.
    pub fn unprotect(&mut self, count: usize) {
        self.protected.truncate(self.protected.len() - count);
    }
}
