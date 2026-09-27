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
