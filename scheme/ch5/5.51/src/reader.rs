//! The reader: turns the characters of a program into Scheme data, one
//! datum at a time, so that the driver loop can evaluate each expression as
//! soon as it has been read.

use std::io::BufRead;

use crate::error::{Error, Result};
use crate::machine::Machine;
use crate::object::Value;

impl Machine {
    /// Reads the next datum, or returns `Value::Eof` when the input has no
    /// more data.
    pub fn read(&mut self, input: &mut dyn BufRead) -> Result<Value> {
        Reader {
            machine: self,
            input,
        }
        .read_datum()
    }
}

struct Reader<'a> {
    machine: &'a mut Machine,
    input: &'a mut dyn BufRead,
}

fn is_space(c: u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\n' | 0x0b | 0x0c | b'\r')
}

fn is_delimiter(c: Option<u8>) -> bool {
    match c {
        None => true,
        Some(c) => is_space(c) || b"()\";'".contains(&c),
    }
}

impl Reader<'_> {
    /// Returns the next character without consuming it. A failure to read
    /// ends the input.
    fn peek(&mut self) -> Option<u8> {
        match self.input.fill_buf() {
            Ok(buffer) => buffer.first().copied(),
            Err(_) => None,
        }
    }

    fn next(&mut self) -> Option<u8> {
        let c = self.peek();
        if c.is_some() {
            self.input.consume(1);
        }
        c
    }

    /// Skips whitespace and comments and returns the first character after
    /// them.
    fn skip_atmosphere(&mut self) -> Option<u8> {
        loop {
            match self.next() {
                Some(b';') => while !matches!(self.next(), Some(b'\n') | None) {},
                Some(c) if is_space(c) => {}
                c => return c,
            }
        }
    }

    fn read_datum(&mut self) -> Result<Value> {
        match self.skip_atmosphere() {
            None => Ok(Value::Eof),
            Some(c) => self.read_datum_starting_with(c),
        }
    }

    fn read_datum_starting_with(&mut self, c: u8) -> Result<Value> {
        match c {
            b'(' => self.read_list(),
            b')' => Err(Error::abort("Unexpected )")),
            b'\'' => self.read_quotation(),
            b'"' => self.read_string(),
            b'#' => self.read_hash_syntax(),
            _ => {
                let token = self.read_token(c);
                Ok(match parse_number(&token) {
                    Some(number) => number,
                    None => self.machine.intern(&token),
                })
            }
        }
    }

    fn read_list(&mut self) -> Result<Value> {
        let machine = &mut *self.machine;
        let head = machine.protect(Value::EmptyList)?;
        let last = machine.protect(Value::EmptyList)?;
        loop {
            match self.skip_atmosphere() {
                None => return Err(Error::abort("Unexpected end of input in a list")),
                Some(b')') => break,
                Some(b'.') if is_delimiter(self.peek()) => {
                    if !self.machine.protected(last).is_pair() {
                        return Err(Error::abort("Nothing before . in a dotted list"));
                    }
                    let tail = self.read_datum()?;
                    let machine = &mut *self.machine;
                    machine.set_cdr(machine.protected(last), tail)?;
                    if self.skip_atmosphere() != Some(b')') {
                        return Err(Error::abort("Expected ) after the tail of a dotted list"));
                    }
                    break;
                }
                Some(c) => {
                    let datum = self.read_datum_starting_with(c)?;
                    let machine = &mut *self.machine;
                    let cell = machine.cons(datum, Value::EmptyList)?;
                    if machine.protected(head).is_null() {
                        machine.set_protected(head, cell);
                    } else {
                        machine.set_cdr(machine.protected(last), cell)?;
                    }
                    machine.set_protected(last, cell);
                }
            }
        }
        let list = self.machine.protected(head);
        self.machine.unprotect(2);
        Ok(list)
    }

    fn read_quotation(&mut self) -> Result<Value> {
        let quoted = self.read_datum()?;
        if quoted == Value::Eof {
            return Err(Error::abort("Unexpected end of input after '"));
        }
        let machine = &mut *self.machine;
        let rest = machine.cons(quoted, Value::EmptyList)?;
        let quote = machine.intern("quote");
        machine.cons(quote, rest)
    }

    fn read_string(&mut self) -> Result<Value> {
        let unexpected_end = || Error::abort("Unexpected end of input in a string");
        let mut bytes = Vec::new();
        loop {
            let c = match self.next().ok_or_else(unexpected_end)? {
                b'"' => break,
                b'\\' => match self.next().ok_or_else(unexpected_end)? {
                    b'n' => b'\n',
                    b't' => b'\t',
                    escaped => escaped,
                },
                c => c,
            };
            bytes.push(c);
        }
        let text = String::from_utf8_lossy(&bytes).into_owned();
        Ok(self.machine.texts.make_string(text))
    }

    /// Reads the rest of a token whose first character, first, was read
    /// already.
    fn read_token(&mut self, first: u8) -> String {
        let mut bytes = vec![first];
        while !is_delimiter(self.peek()) {
            bytes.extend(self.next());
        }
        String::from_utf8_lossy(&bytes).into_owned()
    }

    fn read_hash_syntax(&mut self) -> Result<Value> {
        let token = self.read_token(b'#');
        match token.as_str() {
            "#t" | "#true" => Ok(Value::Boolean(true)),
            "#f" | "#false" => Ok(Value::Boolean(false)),
            _ => {
                let symbol = self.machine.intern(&token);
                Err(self.machine.error("Unknown syntax", symbol))
            }
        }
    }
}

/// Parses integers into fixnums, unless they are too large, and other
/// decimal numbers into flonums. Other tokens are symbols.
fn parse_number(token: &str) -> Option<Value> {
    let is_number_char = |c: char| c.is_ascii_digit() || "+-.e".contains(c);
    if !token.chars().all(is_number_char)
        || !token.chars().any(|c| c.is_ascii_digit())
        || token.starts_with('e')
    {
        return None;
    }
    if let Ok(n) = token.parse::<i64>() {
        return Some(Value::Fixnum(n));
    }
    token.parse::<f64>().ok().map(Value::Flonum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_numbers() {
        assert_eq!(parse_number("42"), Some(Value::Fixnum(42)));
        assert_eq!(parse_number("-7"), Some(Value::Fixnum(-7)));
        assert_eq!(parse_number("+7"), Some(Value::Fixnum(7)));
        assert_eq!(parse_number("2.5"), Some(Value::Flonum(2.5)));
        assert_eq!(parse_number(".5"), Some(Value::Flonum(0.5)));
        assert_eq!(parse_number("-5."), Some(Value::Flonum(-5.0)));
        assert_eq!(parse_number("1e3"), Some(Value::Flonum(1000.0)));
        assert_eq!(parse_number("1e-3"), Some(Value::Flonum(0.001)));
        assert_eq!(
            parse_number("99999999999999999999"),
            Some(Value::Flonum(1e20))
        );
    }

    #[test]
    fn leaves_symbols() {
        for token in ["+", "-", "...", "e1", "1e", "1-2", "1.2.3", "a1", "--1"] {
            assert_eq!(parse_number(token), None, "{token}");
        }
    }
}
