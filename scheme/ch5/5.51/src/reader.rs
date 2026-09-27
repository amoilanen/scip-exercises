use std::io::BufRead;
use std::iter::Peekable;

use crate::machine::{permanent, Machine, Result, Value, Value::*};

pub struct Reader {
    bytes: Peekable<Box<dyn Iterator<Item = u8>>>,
}

impl Reader {
    pub fn new(input: impl BufRead + 'static) -> Reader {
        let bytes: Box<dyn Iterator<Item = u8>> =
            Box::new(input.bytes().map_while(|byte| byte.ok()));
        Reader { bytes: bytes.peekable() }
    }

    /// Returns None at the end of the input.
    pub fn read(&mut self, m: &mut Machine) -> Result<Option<Value>> {
        self.skip_atmosphere();
        if self.bytes.peek().is_none() {
            return Ok(None);
        }
        self.datum(m).map(Some)
    }

    fn skip_atmosphere(&mut self) {
        while let Some(&c) = self.bytes.peek() {
            if c == b';' {
                while self.bytes.next_if(|&c| c != b'\n').is_some() {}
            } else if c.is_ascii_whitespace() {
                self.bytes.next();
            } else {
                return;
            }
        }
    }

    fn next(&mut self) -> Result<u8> {
        self.bytes.next().ok_or_else(|| "Unexpected end of input".into())
    }

    fn datum(&mut self, m: &mut Machine) -> Result<Value> {
        self.skip_atmosphere();
        match self.next()? {
            b'(' => self.list(m),
            b')' => Err("Unexpected )".into()),
            b'\'' => {
                let quoted = self.datum(m)?;
                Ok(m.list(&[Symbol("quote"), quoted], Nil))
            }
            b'"' => self.string(),
            c => Ok(atom(self.token(c))),
        }
    }

    fn list(&mut self, m: &mut Machine) -> Result<Value> {
        let mut items = Vec::new();
        loop {
            self.skip_atmosphere();
            if self.bytes.next_if_eq(&b')').is_some() {
                return Ok(m.list(&items, Nil));
            }
            match self.datum(m)? {
                Symbol(".") if !items.is_empty() => {
                    let tail = self.datum(m)?;
                    self.skip_atmosphere();
                    return match self.next()? {
                        b')' => Ok(m.list(&items, tail)),
                        _ => Err("Expected ) after the tail of a dotted list".into()),
                    };
                }
                item => items.push(item),
            }
        }
    }

    fn string(&mut self) -> Result<Value> {
        let mut bytes = Vec::new();
        loop {
            match self.next()? {
                b'"' => return Ok(Str(permanent(&String::from_utf8_lossy(&bytes)))),
                b'\\' => bytes.push(match self.next()? {
                    b'n' => b'\n',
                    b't' => b'\t',
                    c => c,
                }),
                c => bytes.push(c),
            }
        }
    }

    fn token(&mut self, first: u8) -> String {
        let mut token = vec![first];
        while let Some(c) = self.bytes.next_if(|&c| !is_delimiter(c)) {
            token.push(c);
        }
        String::from_utf8_lossy(&token).into_owned()
    }
}

fn is_delimiter(c: u8) -> bool {
    c.is_ascii_whitespace() || b"()\";'".contains(&c)
}

fn atom(token: String) -> Value {
    let numeric = token.bytes().any(|c| c.is_ascii_digit())
        && token.bytes().all(|c| b"0123456789+-.e".contains(&c));
    let symbol = || Symbol(permanent(&token));
    match token.as_str() {
        "#t" | "#true" => Bool(true),
        "#f" | "#false" => Bool(false),
        _ if numeric => token
            .parse()
            .map(Int)
            .or_else(|_| token.parse().map(Float))
            .unwrap_or_else(|_| symbol()),
        _ => symbol(),
    }
}
