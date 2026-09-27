use super::*;

fn numbers(machine: &mut Machine, numbers: &[i64], tail: Value) -> Value {
    let items: Vec<Value> = numbers.iter().map(|&n| Int(n)).collect();
    machine.list(&items, tail)
}

#[test]
fn car_and_cdr_take_a_pair_apart() {
    let mut machine = Machine::new();
    let pair = machine.cons(Int(1), Int(2));
    assert_eq!(machine.car(pair).unwrap(), Int(1));
    assert_eq!(machine.cdr(pair).unwrap(), Int(2));
}

#[test]
fn set_car_and_set_cdr_change_a_pair() {
    let mut machine = Machine::new();
    let pair = machine.cons(Int(1), Int(2));
    machine.set_car(pair, Int(3)).unwrap();
    machine.set_cdr(pair, Nil).unwrap();
    assert_eq!(machine.show(pair, true), "(3)");
}

#[test]
fn pair_operations_reject_other_objects() {
    let mut machine = Machine::new();
    assert_eq!(machine.car(Nil).unwrap_err().0, "The object passed to car is not a pair: ()");
    assert_eq!(machine.cdr(Int(5)).unwrap_err().0, "The object passed to cdr is not a pair: 5");
    assert_eq!(
        machine.set_car(Symbol("x"), Nil).unwrap_err().0,
        "The object passed to set-car! is not a pair: x"
    );
}

#[test]
fn cxr_applies_its_car_and_cdr_steps_from_right_to_left() {
    let mut machine = Machine::new();
    let inner = numbers(&mut machine, &[2, 3], Nil);
    let list = machine.list(&[Int(1), inner, Int(4)], Nil);
    assert_eq!(machine.show(machine.cxr("cadr", list).unwrap(), true), "(2 3)");
    assert_eq!(machine.cxr("caadr", list).unwrap(), Int(2));
    assert_eq!(machine.show(machine.cxr("cddr", list).unwrap(), true), "(4)");
    assert_eq!(machine.cxr("cadddr", list).unwrap_err().0, "The object passed to car is not a pair: ()");
}

#[test]
fn lists_convert_to_and_from_items() {
    let mut machine = Machine::new();
    let dotted = numbers(&mut machine, &[1, 2], Int(3));
    assert_eq!(machine.items(dotted), (vec![Int(1), Int(2)], Int(3)));
    assert_eq!(machine.to_vec(dotted).unwrap_err().0, "The object is not a list: (1 2 . 3)");
    let proper = numbers(&mut machine, &[1, 2], Nil);
    assert_eq!(machine.to_vec(proper).unwrap(), vec![Int(1), Int(2)]);
    assert_eq!(machine.to_vec(Nil).unwrap(), vec![]);
}

#[test]
fn the_stack_restores_in_reverse_order_and_is_bounded() {
    let mut machine = Machine::new();
    machine.save(Int(1)).unwrap();
    machine.save(Int(2)).unwrap();
    assert_eq!(machine.restore(), Int(2));
    assert_eq!(machine.restore(), Int(1));
    for _ in 0..STACK_SIZE {
        machine.save(Nil).unwrap();
    }
    assert_eq!(machine.save(Nil).unwrap_err().0, "Aborting!: maximum recursion depth exceeded");
}

#[test]
fn garbage_collection_keeps_only_what_the_roots_reach() {
    let mut machine = Machine::new();
    machine.val = numbers(&mut machine, &[1, 2, 3], Nil);
    let list = numbers(&mut machine, &[4, 5], Nil);
    machine.save(list).unwrap();
    let live = machine.memory_used();
    for n in 0..1000 {
        machine.cons(Int(n), Nil);
    }
    machine.collect_garbage().unwrap();
    assert_eq!(machine.memory_used(), live);
    assert_eq!(machine.show(machine.val, true), "(1 2 3)");
    let list = machine.restore();
    assert_eq!(machine.show(list, true), "(4 5)");
    let car = machine.lookup_variable_value(Symbol("car"), machine.global_env).unwrap();
    assert_eq!(car, Primitive("car"));
}

#[test]
fn garbage_collection_preserves_sharing_and_cycles() {
    let mut machine = Machine::new();
    let shared = machine.cons(Int(1), Nil);
    machine.val = machine.cons(shared, shared);
    machine.exp = machine.cons(Int(2), Nil);
    machine.set_cdr(machine.exp, machine.exp).unwrap();
    machine.collect_garbage().unwrap();
    assert_eq!(machine.car(machine.val).unwrap(), machine.cdr(machine.val).unwrap());
    assert_eq!(machine.cdr(machine.exp).unwrap(), machine.exp);
    assert_eq!(machine.car(machine.exp).unwrap(), Int(2));
}

#[test]
fn garbage_collection_moves_procedures_with_their_environments() {
    let mut machine = Machine::new();
    let lambda = numbers(&mut machine, &[1], Nil);
    let env = numbers(&mut machine, &[2], Nil);
    machine.proc = machine.make_procedure(lambda, env);
    machine.collect_garbage().unwrap();
    let Procedure(i) = machine.proc else { panic!("not a procedure: {:?}", machine.proc) };
    let (lambda, env) = machine.procedure_parts(i);
    assert_eq!((machine.show(lambda, true), machine.show(env, true)), ("(1)".into(), "(2)".into()));
}

#[test]
fn garbage_collection_fails_when_live_data_fills_the_memory() {
    let mut machine = Machine::new();
    machine.val = machine.list(&vec![Int(0); MEMORY_SIZE], Nil);
    assert_eq!(machine.collect_garbage().unwrap_err().0, "Aborting!: out of memory");
}

#[test]
fn reset_clears_the_registers_and_the_stack_but_keeps_the_global_environment() {
    let mut machine = Machine::new();
    let global_env = machine.global_env;
    machine.val = Int(1);
    machine.save(Int(2)).unwrap();
    machine.reset();
    assert_eq!(machine.val, Nil);
    assert_eq!(machine.global_env, global_env);
    machine.save(Int(3)).unwrap();
    assert_eq!(machine.restore(), Int(3));
}
