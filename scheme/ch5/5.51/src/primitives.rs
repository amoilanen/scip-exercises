use crate::machine::{Machine, Result, Value, Value::*};

#[rustfmt::skip]
const PRIMITIVES: &[&str] = &[
    "car", "cdr", "caar", "cadr", "cdar", "cddr", "caadr", "cdadr", "caddr", "cdddr", "cadddr",
    "cons", "set-car!", "set-cdr!", "list", "length", "null?", "pair?", "number?", "symbol?",
    "string?", "procedure?", "eq?", "equal?", "not", "+", "-", "*", "/", "=", "<", ">", "<=", ">=",
    "quotient", "remainder", "abs", "display", "newline", "error",
];

impl Machine {
    pub fn setup_environment(&mut self) -> Value {
        let constants = [(Symbol("true"), Bool(true)), (Symbol("false"), Bool(false))];
        let (variables, values): (Vec<_>, Vec<_>) =
            PRIMITIVES.iter().map(|&name| (Symbol(name), Primitive(name))).chain(constants).unzip();
        let frame = self.make_frame(&variables, &values);
        self.cons(frame, Nil)
    }

    pub fn apply_primitive_procedure(
        &mut self,
        name: &'static str,
        args: &[Value],
    ) -> Result<Value> {
        Ok(match (name, args) {
            ("cons", &[car, cdr]) => self.cons(car, cdr),
            ("set-car!", &[pair, value]) => {
                self.set_car(pair, value)?;
                Unspecified
            }
            ("set-cdr!", &[pair, value]) => {
                self.set_cdr(pair, value)?;
                Unspecified
            }
            ("list", _) => self.list(args, Nil),
            ("length", &[list]) => Int(self.to_vec(list)?.len() as i64),
            ("null?", &[x]) => Bool(x == Nil),
            ("pair?", &[x]) => Bool(matches!(x, Pair(_))),
            ("number?", &[x]) => Bool(matches!(x, Int(_) | Float(_))),
            ("symbol?", &[x]) => Bool(matches!(x, Symbol(_))),
            ("string?", &[x]) => Bool(matches!(x, Str(_))),
            ("procedure?", &[x]) => Bool(matches!(x, Primitive(_) | Procedure(_))),
            ("eq?", &[a, b]) => Bool(a == b),
            ("equal?", &[a, b]) => Bool(self.is_equal(a, b)),
            ("not", &[x]) => Bool(x == Bool(false)),
            ("+", _) => self.fold(name, Int(0), args)?,
            ("*", _) => self.fold(name, Int(1), args)?,
            ("-", &[x]) => self.arithmetic(name, Int(0), x)?,
            ("/", &[x]) => self.arithmetic(name, Int(1), x)?,
            ("-" | "/", &[first, ref rest @ ..]) => self.fold(name, first, rest)?,
            ("=" | "<" | ">" | "<=" | ">=", &[_, ..]) => self.compare(name, args)?,
            ("quotient" | "remainder", &[a, b]) => {
                let (x, y) = (self.integer(a)?, self.integer(b)?);
                if y == 0 {
                    return Err("Division by zero signalled by integer division.".into());
                }
                Int(if name == "quotient" { x.wrapping_div(y) } else { x.wrapping_rem(y) })
            }
            ("abs", &[x]) if self.number(x)? < 0.0 => self.arithmetic("-", Int(0), x)?,
            ("abs", &[x]) => x,
            ("display", &[x]) => {
                print!("{}", self.show(x, false));
                Unspecified
            }
            ("newline", []) => {
                println!();
                Unspecified
            }
            ("error", &[Str(message), ref irritants @ ..]) => {
                return Err(self.error(message, irritants))
            }
            ("error", &[_, ..]) => return Err(self.error("Error:", args)),
            (_, &[x]) if name.starts_with('c') && name.ends_with('r') => self.cxr(name, x)?,
            _ => return Err(self.error("Wrong number of arguments passed to", &[Primitive(name)])),
        })
    }

    fn number(&self, x: Value) -> Result<f64> {
        match x {
            Int(n) => Ok(n as f64),
            Float(f) => Ok(f),
            _ => Err(self.error("The object is not a number:", &[x])),
        }
    }

    fn integer(&self, x: Value) -> Result<i64> {
        match x {
            Int(n) => Ok(n),
            _ => Err(self.error("The object is not an integer:", &[x])),
        }
    }

    /// Integers stay exact unless the result overflows or is a fraction.
    fn arithmetic(&self, operator: &str, a: Value, b: Value) -> Result<Value> {
        if operator == "/" && self.number(b)? == 0.0 {
            return Err("Division by zero signalled by /.".into());
        }
        if let (Int(x), Int(y)) = (a, b) {
            let exact = match operator {
                "+" => x.checked_add(y),
                "-" => x.checked_sub(y),
                "*" => x.checked_mul(y),
                _ => x.checked_rem(y).filter(|&r| r == 0).and(x.checked_div(y)),
            };
            if let Some(n) = exact {
                return Ok(Int(n));
            }
        }
        let (x, y) = (self.number(a)?, self.number(b)?);
        Ok(Float(match operator {
            "+" => x + y,
            "-" => x - y,
            "*" => x * y,
            _ => x / y,
        }))
    }

    fn fold(&self, operator: &str, initial: Value, args: &[Value]) -> Result<Value> {
        args.iter().try_fold(initial, |result, &x| self.arithmetic(operator, result, x))
    }

    fn compare(&self, operator: &str, args: &[Value]) -> Result<Value> {
        let numbers = args.iter().map(|&x| self.number(x)).collect::<Result<Vec<_>>>()?;
        Ok(Bool(numbers.windows(2).all(|pair| match operator {
            "=" => pair[0] == pair[1],
            "<" => pair[0] < pair[1],
            ">" => pair[0] > pair[1],
            "<=" => pair[0] <= pair[1],
            _ => pair[0] >= pair[1],
        })))
    }

    fn is_equal(&self, a: Value, b: Value) -> bool {
        let ((xs, x_tail), (ys, y_tail)) = (self.items(a), self.items(b));
        x_tail == y_tail
            && xs.len() == ys.len()
            && xs.iter().zip(&ys).all(|(&x, &y)| self.is_equal(x, y))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn apply_in(machine: &mut Machine, name: &'static str, args: &[Value]) -> String {
        match machine.apply_primitive_procedure(name, args) {
            Ok(value) => machine.show(value, true),
            Err(error) => format!(";{}", error.0),
        }
    }

    fn apply(name: &'static str, args: &[Value]) -> String {
        apply_in(&mut Machine::new(), name, args)
    }

    #[test]
    fn arithmetic_keeps_integers_exact_while_it_can() {
        assert_eq!(apply("+", &[]), "0");
        assert_eq!(apply("+", &[Int(1), Int(2), Int(3)]), "6");
        assert_eq!(apply("-", &[Int(5)]), "-5");
        assert_eq!(apply("-", &[Int(10), Int(4), Int(3)]), "3");
        assert_eq!(apply("*", &[]), "1");
        assert_eq!(apply("*", &[Int(2), Float(3.5)]), "7.");
        assert_eq!(apply("/", &[Int(6), Int(3)]), "2");
        assert_eq!(apply("/", &[Int(1), Int(2)]), ".5");
        assert_eq!(apply("/", &[Int(2)]), ".5");
        assert_eq!(apply("+", &[Int(i64::MAX), Int(1)]), "9223372036854776000.");
    }

    #[test]
    fn division_by_zero_is_an_error() {
        assert_eq!(apply("/", &[Int(1), Int(0)]), ";Division by zero signalled by /.");
        assert_eq!(apply("/", &[Int(1), Float(0.0)]), ";Division by zero signalled by /.");
        assert_eq!(
            apply("remainder", &[Int(1), Int(0)]),
            ";Division by zero signalled by integer division."
        );
    }

    #[test]
    fn integer_division_truncates() {
        assert_eq!(apply("quotient", &[Int(17), Int(5)]), "3");
        assert_eq!(apply("remainder", &[Int(17), Int(5)]), "2");
        assert_eq!(apply("quotient", &[Int(-17), Int(5)]), "-3");
        assert_eq!(apply("remainder", &[Int(-17), Int(5)]), "-2");
        assert_eq!(apply("quotient", &[Float(1.5), Int(1)]), ";The object is not an integer: 1.5");
    }

    #[test]
    fn abs() {
        assert_eq!(apply("abs", &[Int(-3)]), "3");
        assert_eq!(apply("abs", &[Float(-2.5)]), "2.5");
        assert_eq!(apply("abs", &[Int(4)]), "4");
    }

    #[test]
    fn comparisons_hold_for_every_neighbouring_pair() {
        assert_eq!(apply("<", &[Int(1), Int(2), Int(3)]), "#t");
        assert_eq!(apply("<", &[Int(1), Int(3), Int(2)]), "#f");
        assert_eq!(apply("=", &[Int(2), Float(2.0)]), "#t");
        assert_eq!(apply(">=", &[Int(3), Int(3), Int(1)]), "#t");
        assert_eq!(apply("<=", &[Int(2), Int(1)]), "#f");
        assert_eq!(apply(">", &[Int(2), Int(1)]), "#t");
        assert_eq!(apply("=", &[Int(1)]), "#t");
    }

    #[test]
    fn numeric_primitives_take_only_numbers() {
        assert_eq!(apply("+", &[Int(1), Symbol("a")]), ";The object is not a number: a");
        assert_eq!(apply("<", &[Str("a")]), r#";The object is not a number: "a""#);
        assert_eq!(apply("abs", &[Nil]), ";The object is not a number: ()");
    }

    #[test]
    fn primitives_check_the_number_of_arguments() {
        assert_eq!(
            apply("car", &[]),
            ";Wrong number of arguments passed to #[primitive-procedure car]"
        );
        assert_eq!(
            apply("cons", &[Int(1)]),
            ";Wrong number of arguments passed to #[primitive-procedure cons]"
        );
        assert_eq!(
            apply("newline", &[Int(1)]),
            ";Wrong number of arguments passed to #[primitive-procedure newline]"
        );
        assert_eq!(
            apply("-", &[]),
            ";Wrong number of arguments passed to #[primitive-procedure -]"
        );
    }

    #[test]
    fn list_operations() {
        let mut machine = Machine::new();
        let list = machine.list(&[Int(1), Int(2), Int(3)], Nil);
        assert_eq!(apply_in(&mut machine, "list", &[Int(1), Str("s")]), r#"(1 "s")"#);
        assert_eq!(apply_in(&mut machine, "cons", &[Int(0), list]), "(0 1 2 3)");
        assert_eq!(apply_in(&mut machine, "car", &[list]), "1");
        assert_eq!(apply_in(&mut machine, "cddr", &[list]), "(3)");
        assert_eq!(apply_in(&mut machine, "caddr", &[list]), "3");
        assert_eq!(apply_in(&mut machine, "length", &[list]), "3");
        assert_eq!(apply_in(&mut machine, "set-car!", &[list, Int(9)]), "#!unspecific");
        assert_eq!(machine.show(list, true), "(9 2 3)");
        let dotted = machine.cons(Int(1), Int(2));
        assert_eq!(
            apply_in(&mut machine, "length", &[dotted]),
            ";The object is not a list: (1 . 2)"
        );
        assert_eq!(
            apply_in(&mut machine, "cdr", &[Nil]),
            ";The object passed to cdr is not a pair: ()"
        );
    }

    #[test]
    fn type_predicates() {
        let mut machine = Machine::new();
        let pair = machine.cons(Int(1), Nil);
        let procedure = machine.make_procedure(Nil, Nil);
        let cases = [
            ("null?", Nil, "#t"),
            ("null?", pair, "#f"),
            ("pair?", pair, "#t"),
            ("pair?", Nil, "#f"),
            ("number?", Float(1.5), "#t"),
            ("number?", Symbol("x"), "#f"),
            ("symbol?", Symbol("x"), "#t"),
            ("symbol?", Str("x"), "#f"),
            ("string?", Str("x"), "#t"),
            ("procedure?", Primitive("car"), "#t"),
            ("procedure?", procedure, "#t"),
            ("procedure?", Int(1), "#f"),
            ("not", Bool(false), "#t"),
            ("not", Int(0), "#f"),
        ];
        for (name, x, expected) in cases {
            assert_eq!(apply_in(&mut machine, name, &[x]), expected, "({name} {x:?})");
        }
    }

    #[test]
    fn eq_compares_identity_and_equal_compares_structure() {
        let mut machine = Machine::new();
        let a = machine.list(&[Int(1), Str("x")], Nil);
        let b = machine.list(&[Int(1), Str("x")], Nil);
        let longer = machine.list(&[Int(1), Str("x"), Nil], Nil);
        assert_eq!(apply_in(&mut machine, "eq?", &[a, a]), "#t");
        assert_eq!(apply_in(&mut machine, "eq?", &[a, b]), "#f");
        assert_eq!(apply_in(&mut machine, "eq?", &[Symbol("s"), Symbol("s")]), "#t");
        assert_eq!(apply_in(&mut machine, "equal?", &[a, b]), "#t");
        assert_eq!(apply_in(&mut machine, "equal?", &[a, longer]), "#f");
        assert_eq!(apply_in(&mut machine, "equal?", &[Int(2), Float(2.0)]), "#f");
    }

    #[test]
    fn error_reports_its_message_and_irritants() {
        assert_eq!(
            apply("error", &[Str("Something bad:"), Symbol("x"), Int(42), Str("s")]),
            r#";Something bad: x 42 "s""#
        );
        assert_eq!(apply("error", &[Symbol("oops"), Int(1)]), ";Error: oops 1");
    }

    #[test]
    fn the_global_environment_binds_the_primitives_and_the_booleans() {
        let machine = Machine::new();
        let lookup =
            |name| machine.lookup_variable_value(Symbol(name), machine.global_env).unwrap();
        for &name in PRIMITIVES {
            assert_eq!(lookup(name), Primitive(name));
        }
        assert_eq!(lookup("true"), Bool(true));
        assert_eq!(lookup("false"), Bool(false));
    }
}
