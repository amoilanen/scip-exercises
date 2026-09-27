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
