use super::*;
use crate::machine::Error;
use crate::test_support::reader;

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
