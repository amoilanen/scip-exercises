use super::*;

fn symbols(machine: &mut Machine, names: &[&'static str], tail: Value) -> Value {
    let items: Vec<Value> = names.iter().map(|&name| Symbol(name)).collect();
    machine.list(&items, tail)
}

fn lookup(machine: &Machine, name: &'static str, env: Value) -> String {
    match machine.lookup_variable_value(Symbol(name), env) {
        Ok(value) => machine.show(value, true),
        Err(error) => error.0,
    }
}

#[test]
fn extending_binds_the_parameters_to_the_arguments() {
    let mut machine = Machine::new();
    let parameters = symbols(&mut machine, &["x", "y"], Nil);
    let env = machine.extend_environment(parameters, &[Int(1), Int(2)], Nil).unwrap();
    assert_eq!(lookup(&machine, "x", env), "1");
    assert_eq!(lookup(&machine, "y", env), "2");
}

#[test]
fn a_rest_parameter_takes_the_remaining_arguments() {
    let mut machine = Machine::new();
    let parameters = symbols(&mut machine, &["a"], Symbol("rest"));
    let env = machine.extend_environment(parameters, &[Int(1), Int(2), Int(3)], Nil).unwrap();
    assert_eq!(lookup(&machine, "a", env), "1");
    assert_eq!(lookup(&machine, "rest", env), "(2 3)");

    let env = machine.extend_environment(Symbol("args"), &[], Nil).unwrap();
    assert_eq!(lookup(&machine, "args", env), "()");
}

#[test]
fn the_arguments_must_match_the_parameters() {
    let mut machine = Machine::new();
    let parameters = symbols(&mut machine, &["x", "y"], Nil);
    let error = |machine: &mut Machine, parameters, arguments: &[Value]| {
        machine.extend_environment(parameters, arguments, Nil).unwrap_err().0
    };
    assert_eq!(error(&mut machine, parameters, &[Int(1)]), "Too few arguments supplied for (x y)");
    assert_eq!(
        error(&mut machine, parameters, &[Int(1), Int(2), Int(3)]),
        "Too many arguments supplied for (x y)"
    );
    let parameters = symbols(&mut machine, &["x"], Int(5));
    assert_eq!(error(&mut machine, parameters, &[Int(1)]), "Bad parameter list (x . 5)");
}

#[test]
fn lookup_finds_the_innermost_binding() {
    let mut machine = Machine::new();
    let outer = machine.extend_environment(Nil, &[], Nil).unwrap();
    machine.define_variable(Symbol("x"), Int(1), outer).unwrap();
    machine.define_variable(Symbol("y"), Int(2), outer).unwrap();
    let parameters = symbols(&mut machine, &["x"], Nil);
    let inner = machine.extend_environment(parameters, &[Int(3)], outer).unwrap();
    assert_eq!(lookup(&machine, "x", inner), "3");
    assert_eq!(lookup(&machine, "y", inner), "2");
    assert_eq!(lookup(&machine, "x", outer), "1");
    assert_eq!(lookup(&machine, "z", inner), "Unbound variable z");
}

#[test]
fn define_binds_in_the_first_frame_only() {
    let mut machine = Machine::new();
    let outer = machine.extend_environment(Nil, &[], Nil).unwrap();
    let inner = machine.extend_environment(Nil, &[], outer).unwrap();
    machine.define_variable(Symbol("x"), Int(1), inner).unwrap();
    machine.define_variable(Symbol("x"), Int(2), inner).unwrap();
    assert_eq!(lookup(&machine, "x", inner), "2");
    assert_eq!(lookup(&machine, "x", outer), "Unbound variable x");
}

#[test]
fn set_changes_the_innermost_binding() {
    let mut machine = Machine::new();
    let outer = machine.extend_environment(Nil, &[], Nil).unwrap();
    machine.define_variable(Symbol("x"), Int(1), outer).unwrap();
    let inner = machine.extend_environment(Nil, &[], outer).unwrap();
    machine.set_variable_value(Symbol("x"), Int(2), inner).unwrap();
    assert_eq!(lookup(&machine, "x", outer), "2");
    assert_eq!(
        machine.set_variable_value(Symbol("z"), Int(3), inner).unwrap_err().0,
        "Unbound variable -- SET! z"
    );
}
