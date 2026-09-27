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

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;
    use crate::machine::Error;

    fn reader(source: &str) -> Reader {
        Reader::new(Cursor::new(source.to_owned()))
    }

    /// Each datum of the source as write shows it, or the error reading it.
    fn read_all(source: &str) -> Vec<String> {
        let (mut machine, mut reader) = (Machine::new(), reader(source));
        let mut data = Vec::new();
        loop {
            match reader.read(&mut machine) {
                Ok(Some(datum)) => data.push(machine.show(datum, true)),
                Ok(None) => return data,
                Err(Error(message)) => data.push(format!(";{message}")),
            }
        }
    }

    fn read_one(source: &str) -> Value {
        reader(source).read(&mut Machine::new()).unwrap().unwrap()
    }

    #[test]
    fn integers_are_exact_and_other_numbers_inexact() {
        assert_eq!(atom("42".into()), Int(42));
        assert_eq!(atom("-7".into()), Int(-7));
        assert_eq!(atom("+7".into()), Int(7));
        assert_eq!(atom("2.5".into()), Float(2.5));
        assert_eq!(atom(".5".into()), Float(0.5));
        assert_eq!(atom("-5.".into()), Float(-5.0));
        assert_eq!(atom("1e3".into()), Float(1000.0));
        assert_eq!(atom("99999999999999999999".into()), Float(1e20));
    }

    #[test]
    fn tokens_that_are_not_numbers_are_symbols() {
        for token in ["x", "+", "-", "...", "1+", "e1", "1.2.3", "inf", "nan", "set-car!"] {
            assert_eq!(atom(token.into()), Symbol(token), "{token}");
        }
    }

    #[test]
    fn booleans() {
        assert_eq!(atom("#t".into()), Bool(true));
        assert_eq!(atom("#true".into()), Bool(true));
        assert_eq!(atom("#f".into()), Bool(false));
        assert_eq!(atom("#false".into()), Bool(false));
    }

    #[test]
    fn strings_with_escapes() {
        assert_eq!(read_one(r#""a \"b\" \\ \n \t""#), Str("a \"b\" \\ \n \t"));
        assert_eq!(read_one(r#""""#), Str(""));
    }

    #[test]
    fn lists_proper_and_dotted() {
        assert_eq!(
            read_all("() (a b) ( a  b ) (1 (2 . 3) . 4) (a . (b . (c)))"),
            ["()", "(a b)", "(a b)", "(1 (2 . 3) . 4)", "(a b c)"]
        );
    }

    #[test]
    fn quotations() {
        assert_eq!(read_all("'x '(a 'b)"), ["(quote x)", "(quote (a (quote b)))"]);
    }

    #[test]
    fn delimiters_end_tokens() {
        assert_eq!(read_all("a(b)c\"d\"e'f"), ["a", "(b)", "c", "\"d\"", "e", "(quote f)"]);
    }

    #[test]
    fn whitespace_and_comments_separate_data() {
        assert_eq!(read_all("; comment\n 1 ; another\n\t2 ; at the end"), ["1", "2"]);
        assert_eq!(read_all("  ; nothing but a comment"), Vec::<String>::new());
    }

    #[test]
    fn malformed_input_is_reported_and_reading_goes_on() {
        assert_eq!(read_all(") 1"), [";Unexpected )", "1"]);
        assert_eq!(read_all("(1 2"), [";Unexpected end of input"]);
        assert_eq!(read_all("\"abc"), [";Unexpected end of input"]);
        assert_eq!(read_all("'"), [";Unexpected end of input"]);
        assert_eq!(
            read_all("(1 . 2 3) 4"),
            [";Expected ) after the tail of a dotted list", ";Unexpected )", "4"]
        );
    }
}
