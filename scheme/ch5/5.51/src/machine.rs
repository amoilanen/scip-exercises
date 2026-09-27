//! The register machine that runs the evaluator: its memory, stack and
//! registers, and the tables of symbol names and strings.
//!
//! The operations of the machine are spread over the modules that the C
//! version had: `memory.rs` allocates and collects cells, `reader.rs`,
//! `printer.rs`, `environment.rs` and `primitives.rs` provide the data
//! operations, and `eval.rs` the controller.

use std::io::{self, Write};

use crate::error::Result;
use crate::eval::{Keywords, Registers};
use crate::memory::{Heap, STACK_SIZE};
use crate::object::{Texts, Value};

pub struct Machine {
    pub heap: Heap,
    pub stack: Vec<Value>,
    /// Values that functions of the machine hold across allocations; see
    /// `Machine::protect`.
    pub protected: Vec<Value>,
    pub reg: Registers,
    pub global_env: Value,
    pub texts: Texts,
    pub keywords: Keywords,
}

impl Machine {
    /// Makes a machine whose global environment has the primitive
    /// procedures and the variables true and false.
    pub fn new() -> Result<Machine> {
        let mut texts = Texts::default();
        let keywords = Keywords::new(&mut texts);
        let mut machine = Machine {
            heap: Heap::new(),
            stack: Vec::with_capacity(STACK_SIZE),
            protected: Vec::new(),
            reg: Registers::new(),
            global_env: Value::EmptyList,
            texts,
            keywords,
        };
        machine.global_env = machine.setup_environment()?;
        Ok(machine)
    }

    pub fn intern(&mut self, name: &str) -> Value {
        self.texts.intern(name)
    }

    /// Writes text on standard output. A closed output is not an error of
    /// the Scheme program, so failures are ignored.
    pub fn output(&self, text: &str) {
        let _ = io::stdout().write_all(text.as_bytes());
    }
}
