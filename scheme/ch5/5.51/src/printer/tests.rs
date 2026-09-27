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
