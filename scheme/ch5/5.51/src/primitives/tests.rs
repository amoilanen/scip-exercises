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
    assert_eq!(apply("-", &[]), ";Wrong number of arguments passed to #[primitive-procedure -]");
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
    assert_eq!(apply_in(&mut machine, "length", &[dotted]), ";The object is not a list: (1 . 2)");
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
    let lookup = |name| machine.lookup_variable_value(Symbol(name), machine.global_env).unwrap();
    for &name in PRIMITIVES {
        assert_eq!(lookup(name), Primitive(name));
    }
    assert_eq!(lookup("true"), Bool(true));
    assert_eq!(lookup("false"), Bool(false));
}
