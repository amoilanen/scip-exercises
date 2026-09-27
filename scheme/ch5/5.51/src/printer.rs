//! The printer. write prints strings in double quotes, so that the reader
//! can read them back; display prints their characters only.

use crate::machine::Machine;
use crate::object::Value;
use crate::primitives::PRIMITIVES;

impl Machine {
    pub fn write_to_string(&self, v: Value) -> String {
        let mut out = String::new();
        self.print(&mut out, v, true);
        out
    }

    pub fn display_to_string(&self, v: Value) -> String {
        let mut out = String::new();
        self.print(&mut out, v, false);
        out
    }

    fn print(&self, out: &mut String, v: Value, write: bool) {
        match v {
            Value::EmptyList => out.push_str("()"),
            Value::Boolean(b) => out.push_str(if b { "#t" } else { "#f" }),
            Value::Fixnum(n) => out.push_str(&n.to_string()),
            Value::Flonum(x) => out.push_str(&format_flonum(x)),
            Value::Symbol(index) => out.push_str(self.texts.symbol_name(index)),
            Value::Str(index) => {
                let text = self.texts.string_text(index);
                if write {
                    print_string(out, text);
                } else {
                    out.push_str(text);
                }
            }
            Value::Pair(_) => self.print_list(out, v, write),
            Value::Primitive(index) => {
                out.push_str(&format!(
                    "#[primitive-procedure {}]",
                    PRIMITIVES[index].name
                ));
            }
            Value::Procedure(_) => out.push_str("#[compound-procedure]"),
            Value::Label(label) => out.push_str(&format!("#[label {}]", label as u32)),
            Value::Unspecified => out.push_str("#!unspecific"),
            Value::Eof => out.push_str("#[eof]"),
            Value::BrokenHeart(_) => out.push_str("#[broken-heart]"),
        }
    }

    fn print_list(&self, out: &mut String, list: Value, write: bool) {
        out.push('(');
        self.print(out, self.cell_car(list), write);
        let mut rest = self.cell_cdr(list);
        while rest.is_pair() {
            out.push(' ');
            self.print(out, self.cell_car(rest), write);
            rest = self.cell_cdr(rest);
        }
        if !rest.is_null() {
            out.push_str(" . ");
            self.print(out, rest, write);
        }
        out.push(')');
    }
}

fn print_string(out: &mut String, text: &str) {
    out.push('"');
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            _ => out.push(c),
        }
    }
    out.push('"');
}

/// Prints the shortest digits that read back as the same number, in the
/// style of MIT Scheme: 3. for 3.0, .5 for 0.5 and 1e21 for 1e+21.
fn format_flonum(x: f64) -> String {
    if x.is_nan() {
        return "+nan.0".to_owned();
    }
    if x.is_infinite() {
        return if x > 0.0 { "+inf.0" } else { "-inf.0" }.to_owned();
    }

    // {:e} gives the shortest digits that read back as x, as [-]d.ddde[-]x.
    let scientific = format!("{x:e}");
    let (mantissa, exponent) = scientific
        .split_once('e')
        .expect("the exponent of a flonum");
    let exponent: i32 = exponent.parse().expect("the exponent of a flonum");
    let (sign, mantissa) = match mantissa.strip_prefix('-') {
        Some(magnitude) => ("-", magnitude),
        None => ("", mantissa),
    };
    let digits: String = mantissa.chars().filter(|&c| c != '.').collect();
    let digits = match digits.trim_end_matches('0') {
        "" => "0",
        significant => significant,
    };

    let mut out = sign.to_owned();
    if !(-6..21).contains(&exponent) {
        let (first, rest) = digits.split_at(1);
        out.push_str(first);
        if !rest.is_empty() {
            out.push('.');
            out.push_str(rest);
        }
        out.push_str(&format!("e{exponent}"));
    } else if exponent < 0 {
        out.push('.');
        out.push_str(&"0".repeat((-exponent - 1) as usize));
        out.push_str(digits);
    } else {
        let integer_digits = exponent as usize + 1;
        if digits.len() > integer_digits {
            out.push_str(&digits[..integer_digits]);
            out.push('.');
            out.push_str(&digits[integer_digits..]);
        } else {
            out.push_str(digits);
            out.push_str(&"0".repeat(integer_digits - digits.len()));
            out.push('.');
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::format_flonum;

    #[test]
    fn formats_flonums_like_mit_scheme() {
        let cases = [
            (2.5, "2.5"),
            (0.5, ".5"),
            (3.0, "3."),
            (-3.0, "-3."),
            (0.0, "0."),
            (-0.0, "-0."),
            (100.0, "100."),
            (123.456, "123.456"),
            (0.001, ".001"),
            (1e-7, "1e-7"),
            (-1.5e-7, "-1.5e-7"),
            (1e20, "100000000000000000000."),
            (1e21, "1e21"),
            (1.25e22, "1.25e22"),
            (0.1 + 0.2, ".30000000000000004"),
            (f64::INFINITY, "+inf.0"),
            (f64::NEG_INFINITY, "-inf.0"),
            (f64::NAN, "+nan.0"),
        ];
        for (x, expected) in cases {
            assert_eq!(format_flonum(x), expected, "{x}");
        }
    }
}
