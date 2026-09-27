use crate::test_support::run;

#[test]
fn self_evaluating_expressions_and_quotations() {
    assert_eq!(
        run("42 -2.5 \"s\" #t 'x '(1 \"s\")"),
        ["42", "-2.5", "\"s\"", "#t", "x", "(1 \"s\")"]
    );
}

#[test]
fn definitions_bind_variables_and_procedures() {
    assert_eq!(run("(define x 1) x (define (f y) (+ x y)) (f 2)"), ["ok", "1", "ok", "3"]);
}

#[test]
fn lambda_makes_closures() {
    let program = "(define (make-counter)
                     (let ((count 0)) (lambda () (set! count (+ count 1)) count)))
                   (define c (make-counter))
                   (c) (c) ((make-counter))";
    assert_eq!(run(program), ["ok", "ok", "1", "2", "1"]);
}

#[test]
fn application_evaluates_operands_from_left_to_right() {
    let program = "(define n 0) (define (next!) (set! n (+ n 1)) n) (list (next!) (next!))";
    assert_eq!(run(program), ["ok", "ok", "(1 2)"]);
}

#[test]
fn application_binds_rest_parameters() {
    assert_eq!(
        run("((lambda (a . rest) (list a rest)) 1 2 3) ((lambda args args))"),
        ["(1 (2 3))", "()"]
    );
}

#[test]
fn if_evaluates_one_branch() {
    assert_eq!(
        run("(if #t 1 (car '())) (if #f (car '()) 2) (if 0 'true) (if #f 1)"),
        ["1", "2", "true"]
    );
}

#[test]
fn cond_evaluates_the_first_clause_whose_test_holds() {
    assert_eq!(
        run("(cond ((= 1 2) 'a) ((= 1 1) 'b) ((car '()) 'c))
             (cond (#f 1) (else 2 3))
             (cond (#f 1))
             (cond (#f 1) ((+ 1 1)))"),
        ["b", "3", "2"]
    );
}

#[test]
fn let_binds_its_variables_in_parallel() {
    assert_eq!(run("(let ((x 1) (y 2)) (let ((x y) (y x)) (list x y)))"), ["(2 1)"]);
}

#[test]
fn begin_and_bodies_evaluate_in_sequence() {
    assert_eq!(run("(begin 1 2 3) (define (f) 1 2 3) (f)"), ["3", "ok", "3"]);
}

#[test]
fn set_changes_existing_variables_only() {
    assert_eq!(
        run("(define x 1) (set! x 2) x (set! y 1)"),
        ["ok", "ok", "2", ";Unbound variable -- SET! y"]
    );
}

#[test]
fn recursion() {
    let program = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 20)";
    assert_eq!(run(program), ["ok", "2432902008176640000"]);
}

#[test]
fn iterative_processes_run_in_constant_space() {
    let program = "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (loop 200000)";
    assert_eq!(run(program), ["ok", "done"]);
}

#[test]
fn deep_recursion_aborts_and_the_machine_recovers() {
    let program = "(define (count n) (if (= n 0) 0 (+ 1 (count (- n 1)))))
                   (count 100000)
                   (count 100)";
    assert_eq!(run(program), ["ok", ";Aborting!: maximum recursion depth exceeded", "100"]);
}

#[test]
fn errors_are_reported_and_evaluation_goes_on() {
    assert_eq!(
        run("(undefined) (1 2) ((lambda (x) x)) () (car '()) 'next"),
        [
            ";Unbound variable undefined",
            ";The object is not applicable: 1",
            ";Too few arguments supplied for (x)",
            ";Unknown expression type ()",
            ";The object passed to car is not a pair: ()",
            "next",
        ]
    );
}

#[test]
fn garbage_collection_keeps_live_data() {
    let program = "(define (iota n) (if (= n 0) '() (cons n (iota (- n 1)))))
                   (define (sum list) (if (null? list) 0 (+ (car list) (sum (cdr list)))))
                   (define keep (iota 1000))
                   (define (churn k) (if (= k 0) 'done (begin (iota 1000) (churn (- k 1)))))
                   (churn 300)
                   (sum keep)";
    assert_eq!(run(program), ["ok", "ok", "ok", "ok", "done", "500500"]);
}

#[test]
fn running_out_of_memory_aborts_and_the_machine_recovers() {
    let program = "(define (grow list) (grow (cons list list))) (grow '()) 'recovered";
    assert_eq!(run(program), ["ok", ";Aborting!: out of memory", "recovered"]);
}
