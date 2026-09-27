//! Scheme objects as typed pointers (section 5.3.1).
//!
//! A `Value` is a type tag plus a datum. Numbers, booleans, symbols and the
//! other atoms are held in the value itself. Pairs and procedures point into
//! the list-structured memory of `memory.rs` by index.

use std::collections::HashMap;

use crate::eval::Label;

/// The derived equality is `eq?`: atoms are equal when their data are, and
/// pairs and procedures when they are the same cell. Symbols are interned,
/// so they are equal exactly when their names are.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Value {
    EmptyList,
    Boolean(bool),
    Fixnum(i64),
    Flonum(f64),
    /// An index into the symbol table of `Texts`.
    Symbol(usize),
    /// An index into the string table of `Texts`.
    Str(usize),
    /// An index into `primitives::PRIMITIVES`.
    Primitive(usize),
    Label(Label),
    Unspecified,
    Eof,
    // These point into memory, where the garbage collector moves them.
    Pair(usize),
    /// A compound procedure: a cell holding its lambda expression and the
    /// environment it was made in.
    Procedure(usize),
    /// Marks a cell that the garbage collector has moved, and holds the
    /// index of the cell it moved to.
    BrokenHeart(usize),
}

impl Value {
    pub fn is_false(self) -> bool {
        self == Value::Boolean(false)
    }

    pub fn is_true(self) -> bool {
        !self.is_false()
    }

    pub fn is_null(self) -> bool {
        self == Value::EmptyList
    }

    pub fn is_pair(self) -> bool {
        matches!(self, Value::Pair(_))
    }

    pub fn is_symbol(self) -> bool {
        matches!(self, Value::Symbol(_))
    }

    pub fn is_number(self) -> bool {
        matches!(self, Value::Fixnum(_) | Value::Flonum(_))
    }
}

/// Symbol names and string contents live outside the garbage-collected
/// memory. Strings come only from the reader, so they are never freed.
#[derive(Default)]
pub struct Texts {
    symbols: Vec<String>,
    symbol_indices: HashMap<String, usize>,
    strings: Vec<String>,
}

impl Texts {
    pub fn intern(&mut self, name: &str) -> Value {
        if let Some(&index) = self.symbol_indices.get(name) {
            return Value::Symbol(index);
        }
        let index = self.symbols.len();
        self.symbols.push(name.to_owned());
        self.symbol_indices.insert(name.to_owned(), index);
        Value::Symbol(index)
    }

    pub fn make_string(&mut self, text: String) -> Value {
        self.strings.push(text);
        Value::Str(self.strings.len() - 1)
    }

    pub fn symbol_name(&self, index: usize) -> &str {
        &self.symbols[index]
    }

    pub fn string_text(&self, index: usize) -> &str {
        &self.strings[index]
    }
}
