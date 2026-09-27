use crate::machine::{Machine, Value, Value::*};

impl Machine {
    /// write shows strings in quotes, so that the reader can read them back.
    pub fn show(&self, v: Value, write: bool) -> String {
        match v {
            Nil => "()".into(),
            Bool(b) => if b { "#t" } else { "#f" }.into(),
            Int(n) => n.to_string(),
            Float(x) => show_float(x),
            Symbol(name) => name.into(),
            Str(text) if write => format!("{text:?}"),
            Str(text) => text.into(),
            Pair(_) => self.show_list(v, write),
            Primitive(name) => format!("#[primitive-procedure {name}]"),
            Procedure(_) => "#[compound-procedure]".into(),
            Unspecified => "#!unspecific".into(),
            Label(_) | BrokenHeart(_) => format!("#[{v:?}]"),
        }
    }

    fn show_list(&self, list: Value, write: bool) -> String {
        let (items, tail) = self.items(list);
        let mut parts: Vec<String> = items.into_iter().map(|item| self.show(item, write)).collect();
        if tail != Nil {
            parts.push(format!(". {}", self.show(tail, write)));
        }
        format!("({})", parts.join(" "))
    }
}

/// In the style of MIT Scheme: 3. for 3.0 and .5 for 0.5.
fn show_float(x: f64) -> String {
    if x.fract() == 0.0 {
        format!("{x}.")
    } else if x < 0.0 {
        format!("-{}", show_float(-x))
    } else {
        x.to_string().trim_start_matches('0').into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atoms() {
        let machine = Machine::new();
        let shown =
            [Nil, Bool(true), Bool(false), Int(-7), Symbol("sym"), Unspecified, Primitive("car")]
                .map(|v| machine.show(v, true));
        assert_eq!(
            shown,
            ["()", "#t", "#f", "-7", "sym", "#!unspecific", "#[primitive-procedure car]"]
        );
    }

    #[test]
    fn floats_in_the_style_of_mit_scheme() {
        let cases = [
            (2.5, "2.5"),
            (0.5, ".5"),
            (-0.5, "-.5"),
            (0.001, ".001"),
            (3.0, "3."),
            (-3.0, "-3."),
            (0.0, "0."),
            (100.0, "100."),
            (123.456, "123.456"),
        ];
        for (x, expected) in cases {
            assert_eq!(show_float(x), expected, "{x}");
        }
    }

    #[test]
    fn write_quotes_strings_and_display_does_not() {
        let machine = Machine::new();
        let text = Str("say \"hi\"\n");
        assert_eq!(machine.show(text, true), r#""say \"hi\"\n""#);
        assert_eq!(machine.show(text, false), "say \"hi\"\n");
    }

    #[test]
    fn lists_proper_and_dotted() {
        let mut machine = Machine::new();
        let pair = machine.cons(Int(2), Int(3));
        let list = machine.list(&[Int(1), pair, Str("s")], Int(4));
        assert_eq!(machine.show(list, true), r#"(1 (2 . 3) "s" . 4)"#);
        assert_eq!(machine.show(list, false), "(1 (2 . 3) s . 4)");
    }

    #[test]
    fn procedures() {
        let mut machine = Machine::new();
        let procedure = machine.make_procedure(Nil, Nil);
        assert_eq!(machine.show(procedure, true), "#[compound-procedure]");
    }
}
