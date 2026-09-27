//! The primitive procedures, and the global environment that binds them.
//!
//! Each primitive gets its arguments as a list, whose length
//! `apply_primitive_procedure` has checked against the primitive's arity.

use std::cmp::Ordering;

use crate::error::{Error, Result};
use crate::machine::Machine;
use crate::object::Value;

pub struct Primitive {
    pub name: &'static str,
    /// The number of arguments, or the minimum if variadic.
    pub arity: usize,
    pub variadic: bool,
    pub function: fn(&mut Machine, Value) -> Result<Value>,
}

const fn fixed(
    name: &'static str,
    arity: usize,
    function: fn(&mut Machine, Value) -> Result<Value>,
) -> Primitive {
    Primitive {
        name,
        arity,
        variadic: false,
        function,
    }
}

const fn variadic(
    name: &'static str,
    arity: usize,
    function: fn(&mut Machine, Value) -> Result<Value>,
) -> Primitive {
    Primitive {
        name,
        arity,
        variadic: true,
        function,
    }
}

pub static PRIMITIVES: &[Primitive] = &[
    fixed("car", 1, |m, args| m.car(m.car(args)?)),
    fixed("cdr", 1, |m, args| m.cdr(m.car(args)?)),
    fixed("caar", 1, |m, args| m.caar(m.car(args)?)),
    fixed("cadr", 1, |m, args| m.cadr(m.car(args)?)),
    fixed("cdar", 1, |m, args| m.cdar(m.car(args)?)),
    fixed("cddr", 1, |m, args| m.cddr(m.car(args)?)),
    fixed("caadr", 1, |m, args| m.car(m.cadr(m.car(args)?)?)),
    fixed("cdadr", 1, |m, args| m.cdr(m.cadr(m.car(args)?)?)),
    fixed("caddr", 1, |m, args| m.caddr(m.car(args)?)),
    fixed("cdddr", 1, |m, args| m.cdddr(m.car(args)?)),
    fixed("cadddr", 1, |m, args| m.car(m.cdddr(m.car(args)?)?)),
    fixed("cons", 2, |m, args| {
        let (car, cdr) = (m.car(args)?, m.cadr(args)?);
        m.cons(car, cdr)
    }),
    fixed("set-car!", 2, |m, args| {
        let (pair, value) = (m.car(args)?, m.cadr(args)?);
        m.set_car(pair, value)?;
        Ok(Value::Unspecified)
    }),
    fixed("set-cdr!", 2, |m, args| {
        let (pair, value) = (m.car(args)?, m.cadr(args)?);
        m.set_cdr(pair, value)?;
        Ok(Value::Unspecified)
    }),
    // The argument list is always freshly made, so it can be the result.
    variadic("list", 0, |_, args| Ok(args)),
    fixed("length", 1, prim_length),
    fixed("null?", 1, |m, args| {
        Ok(Value::Boolean(m.car(args)?.is_null()))
    }),
    fixed("pair?", 1, |m, args| {
        Ok(Value::Boolean(m.car(args)?.is_pair()))
    }),
    fixed("number?", 1, |m, args| {
        Ok(Value::Boolean(m.car(args)?.is_number()))
    }),
    fixed("symbol?", 1, |m, args| {
        Ok(Value::Boolean(m.car(args)?.is_symbol()))
    }),
    fixed("string?", 1, |m, args| {
        Ok(Value::Boolean(matches!(m.car(args)?, Value::Str(_))))
    }),
    fixed("procedure?", 1, |m, args| {
        Ok(Value::Boolean(matches!(
            m.car(args)?,
            Value::Primitive(_) | Value::Procedure(_)
        )))
    }),
    fixed("eq?", 2, |m, args| {
        Ok(Value::Boolean(m.car(args)? == m.cadr(args)?))
    }),
    fixed("equal?", 2, |m, args| {
        Ok(Value::Boolean(is_equal(m, m.car(args)?, m.cadr(args)?)))
    }),
    fixed("not", 1, |m, args| {
        Ok(Value::Boolean(m.car(args)?.is_false()))
    }),
    variadic("+", 0, |m, args| {
        fold_numbers(m, add, Value::Fixnum(0), args)
    }),
    variadic("-", 1, |m, args| {
        let first = m.car(args)?;
        let rest = m.cdr(args)?;
        if rest.is_null() {
            subtract(m, Value::Fixnum(0), first)
        } else {
            fold_numbers(m, subtract, first, rest)
        }
    }),
    variadic("*", 0, |m, args| {
        fold_numbers(m, multiply, Value::Fixnum(1), args)
    }),
    variadic("/", 1, |m, args| {
        let first = m.car(args)?;
        let rest = m.cdr(args)?;
        if rest.is_null() {
            divide(m, Value::Fixnum(1), first)
        } else {
            fold_numbers(m, divide, first, rest)
        }
    }),
    variadic("=", 1, |m, args| compare_all(m, args, Ordering::is_eq)),
    variadic("<", 1, |m, args| compare_all(m, args, Ordering::is_lt)),
    variadic(">", 1, |m, args| compare_all(m, args, Ordering::is_gt)),
    variadic("<=", 1, |m, args| compare_all(m, args, Ordering::is_le)),
    variadic(">=", 1, |m, args| compare_all(m, args, Ordering::is_ge)),
    fixed("quotient", 2, |m, args| integer_division(m, args, true)),
    fixed("remainder", 2, |m, args| integer_division(m, args, false)),
    fixed("abs", 1, |m, args| {
        let x = m.car(args)?;
        if to_double(m, x)? < 0.0 {
            subtract(m, Value::Fixnum(0), x)
        } else {
            Ok(x)
        }
    }),
    fixed("display", 1, |m, args| {
        let text = m.display_to_string(m.car(args)?);
        m.output(&text);
        Ok(Value::Unspecified)
    }),
    fixed("newline", 0, |m, _| {
        m.output("\n");
        Ok(Value::Unspecified)
    }),
    variadic("error", 1, |m, args| match m.car(args)? {
        Value::Str(index) => Err(m.error_list(m.texts.string_text(index), m.cdr(args)?)),
        _ => Err(m.error_list("Error:", args)),
    }),
];

/*** Numbers ***/

fn to_double(m: &Machine, v: Value) -> Result<f64> {
    match v {
        Value::Fixnum(n) => Ok(n as f64),
        Value::Flonum(x) => Ok(x),
        _ => Err(m.error("The object is not a number:", v)),
    }
}

fn to_integer(m: &Machine, v: Value) -> Result<i64> {
    match v {
        Value::Fixnum(n) => Ok(n),
        _ => Err(m.error("The object is not an integer:", v)),
    }
}

fn integer_overflow() -> Error {
    Error::abort("Integer overflow")
}

type Operation = fn(&Machine, Value, Value) -> Result<Value>;

fn add(m: &Machine, a: Value, b: Value) -> Result<Value> {
    if let (Value::Fixnum(x), Value::Fixnum(y)) = (a, b) {
        return x
            .checked_add(y)
            .map(Value::Fixnum)
            .ok_or_else(integer_overflow);
    }
    Ok(Value::Flonum(to_double(m, a)? + to_double(m, b)?))
}

fn subtract(m: &Machine, a: Value, b: Value) -> Result<Value> {
    if let (Value::Fixnum(x), Value::Fixnum(y)) = (a, b) {
        return x
            .checked_sub(y)
            .map(Value::Fixnum)
            .ok_or_else(integer_overflow);
    }
    Ok(Value::Flonum(to_double(m, a)? - to_double(m, b)?))
}

fn multiply(m: &Machine, a: Value, b: Value) -> Result<Value> {
    if let (Value::Fixnum(x), Value::Fixnum(y)) = (a, b) {
        return x
            .checked_mul(y)
            .map(Value::Fixnum)
            .ok_or_else(integer_overflow);
    }
    Ok(Value::Flonum(to_double(m, a)? * to_double(m, b)?))
}

/// Integer division gives an integer only when it is exact.
fn divide(m: &Machine, a: Value, b: Value) -> Result<Value> {
    let divisor = to_double(m, b)?;
    if divisor == 0.0 {
        return Err(Error::abort("Division by zero signalled by /."));
    }
    if let (Value::Fixnum(x), Value::Fixnum(y)) = (a, b) {
        if x.checked_rem(y) == Some(0) {
            return Ok(Value::Fixnum(x / y));
        }
    }
    Ok(Value::Flonum(to_double(m, a)? / divisor))
}

fn fold_numbers(
    m: &Machine,
    operation: Operation,
    initial: Value,
    arguments: Value,
) -> Result<Value> {
    let mut result = initial;
    let mut list = arguments;
    while list.is_pair() {
        result = operation(m, result, m.cell_car(list))?;
        list = m.cell_cdr(list);
    }
    Ok(result)
}

fn integer_division(m: &Machine, args: Value, quotient: bool) -> Result<Value> {
    let dividend = m.car(args)?;
    let x = to_integer(m, dividend)?;
    let y = to_integer(m, m.cadr(args)?)?;
    if y == 0 {
        return Err(Error::abort(
            "Division by zero signalled by integer division.",
        ));
    }
    if y == -1 {
        // Dividing the most negative fixnum by -1 overflows.
        return if quotient {
            subtract(m, Value::Fixnum(0), dividend)
        } else {
            Ok(Value::Fixnum(0))
        };
    }
    Ok(Value::Fixnum(if quotient { x / y } else { x % y }))
}

/// Unordered flonums, that is NaNs, compare equal.
fn compare(m: &Machine, a: Value, b: Value) -> Result<Ordering> {
    if let (Value::Fixnum(x), Value::Fixnum(y)) = (a, b) {
        return Ok(x.cmp(&y));
    }
    let (x, y) = (to_double(m, a)?, to_double(m, b)?);
    Ok(x.partial_cmp(&y).unwrap_or(Ordering::Equal))
}

/// Compares every pair of neighbouring arguments, so that each of them is
/// checked to be a number.
fn compare_all(m: &Machine, args: Value, relation: fn(Ordering) -> bool) -> Result<Value> {
    let first = m.car(args)?;
    if !first.is_number() {
        return Err(m.error("The object is not a number:", first));
    }
    let mut holds = true;
    let mut list = args;
    while m.cell_cdr(list).is_pair() {
        let next = m.cell_cdr(list);
        if !relation(compare(m, m.cell_car(list), m.cell_car(next))?) {
            holds = false;
        }
        list = next;
    }
    Ok(Value::Boolean(holds))
}

/*** Lists ***/

fn prim_length(m: &mut Machine, args: Value) -> Result<Value> {
    let mut length = 0;
    let mut list = m.car(args)?;
    while list.is_pair() {
        length += 1;
        list = m.cell_cdr(list);
    }
    if !list.is_null() {
        return Err(m.error("The object is not a list:", m.car(args)?));
    }
    Ok(Value::Fixnum(length))
}

fn is_equal(m: &Machine, a: Value, b: Value) -> bool {
    let (mut a, mut b) = (a, b);
    while a.is_pair() && b.is_pair() {
        if !is_equal(m, m.cell_car(a), m.cell_car(b)) {
            return false;
        }
        a = m.cell_cdr(a);
        b = m.cell_cdr(b);
    }
    if let (Value::Str(i), Value::Str(j)) = (a, b) {
        return m.texts.string_text(i) == m.texts.string_text(j);
    }
    a == b
}

/*** The global environment ***/

impl Machine {
    /// Makes a global environment with the primitive procedures and the
    /// variables true and false.
    pub fn setup_environment(&mut self) -> Result<Value> {
        let env = self.extend_environment(Value::EmptyList, Value::EmptyList, Value::EmptyList)?;
        let env = self.protect(env)?;
        for (index, primitive) in PRIMITIVES.iter().enumerate() {
            let name = self.intern(primitive.name);
            self.define_variable(name, Value::Primitive(index), self.protected(env))?;
        }
        let name = self.intern("true");
        self.define_variable(name, Value::Boolean(true), self.protected(env))?;
        let name = self.intern("false");
        self.define_variable(name, Value::Boolean(false), self.protected(env))?;
        let global_env = self.protected(env);
        self.unprotect(1);
        Ok(global_env)
    }

    pub fn apply_primitive_procedure(&mut self, index: usize, arguments: Value) -> Result<Value> {
        let primitive = &PRIMITIVES[index];
        let mut count = 0;
        let mut list = arguments;
        while list.is_pair() {
            count += 1;
            list = self.cell_cdr(list);
        }
        if count < primitive.arity || (count > primitive.arity && !primitive.variadic) {
            return Err(self.error(
                "Wrong number of arguments passed to",
                Value::Primitive(index),
            ));
        }
        (primitive.function)(self, arguments)
    }
}
