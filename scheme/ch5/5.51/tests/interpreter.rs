//! Runs the interpreter as a program, the way a user does.

use std::io::Write;
use std::process::{Command, Stdio};
use std::{env, fs, process};

struct Output {
    stdout: String,
    stderr: String,
    success: bool,
}

fn scheme(args: &[&str], input: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_scheme"))
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("the interpreter starts");
    // The interpreter may exit without reading its input, which closes the pipe.
    let _ = child.stdin.take().unwrap().write_all(input.as_bytes());
    let output = child.wait_with_output().unwrap();
    Output {
        stdout: String::from_utf8(output.stdout).unwrap(),
        stderr: String::from_utf8(output.stderr).unwrap(),
        success: output.status.success(),
    }
}

#[test]
fn prints_the_value_of_each_expression() {
    let output = scheme(&[], "(define x 2) (* x 21) 'sym \"str\" (list 1 2.5)");
    assert_eq!(output.stdout, "ok\n42\nsym\n\"str\"\n(1 2.5)\n");
    assert_eq!(output.stderr, "");
    assert!(output.success);
}

#[test]
fn prints_only_what_display_shows_for_unspecified_values() {
    let output = scheme(&[], "(display \"x = \") (display '(\"s\" 1)) (newline) (if #f #f)");
    assert_eq!(output.stdout, "x = (s 1)\n");
    assert!(output.success);
}

#[test]
fn reports_errors_on_standard_error_goes_on_and_fails_in_the_end() {
    let output = scheme(&[], "(car '()) (error \"Something bad:\" 'x 42) ) 'next");
    assert_eq!(output.stdout, "next\n");
    assert_eq!(
        output.stderr,
        ";The object passed to car is not a pair: ()\n;Something bad: x 42\n;Unexpected )\n"
    );
    assert!(!output.success);
}

#[test]
fn reports_input_that_ends_in_the_middle_of_an_expression() {
    let output = scheme(&[], "1 (+ 1");
    assert_eq!(output.stdout, "1\n");
    assert_eq!(output.stderr, ";Unexpected end of input\n");
    assert!(!output.success);
}

#[test]
fn reads_the_program_from_a_file() {
    let path = env::temp_dir().join(format!("scheme-5.51-{}.scm", process::id()));
    fs::write(&path, "(define (square x) (* x x))\n(square 12)\n").unwrap();
    let output = scheme(&[path.to_str().unwrap()], "'ignored");
    fs::remove_file(&path).unwrap();
    assert_eq!(output.stdout, "ok\n144\n");
    assert!(output.success);
}

#[test]
fn fails_for_a_missing_file() {
    let output = scheme(&["/nonexistent/program.scm"], "");
    assert!(output.stderr.starts_with("/nonexistent/program.scm: "));
    assert!(!output.success);
}

#[test]
fn runs_the_programs_of_the_book() {
    let program = "
        (define (make-account balance)
          (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                \"Insufficient funds\"))
          withdraw)
        (define acc (make-account 100))
        (acc 50)
        (acc 60)
        (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
        (fib 20)
        (define (sqrt-iter guess x)
          (if (< (abs (- (* guess guess) x)) 0.001)
              guess
              (sqrt-iter (/ (+ guess (/ x guess)) 2) x)))
        (< (abs (- (sqrt-iter 1.0 2) 1.41421)) 0.0001)";
    let output = scheme(&[], program);
    assert_eq!(output.stdout, "ok\nok\n50\n\"Insufficient funds\"\nok\n6765\nok\n#t\n");
    assert!(output.success);
}

#[test]
fn keeps_running_after_the_limits_of_the_machine() {
    let program = "
        (define (count n) (if (= n 0) 0 (+ 1 (count (- n 1)))))
        (count 100000)
        (define (grow list) (grow (cons list list)))
        (grow '())
        (count 1000)";
    let output = scheme(&[], program);
    assert_eq!(output.stdout, "ok\nok\n1000\n");
    assert_eq!(
        output.stderr,
        ";Aborting!: maximum recursion depth exceeded\n;Aborting!: out of memory\n"
    );
    assert!(!output.success);
}
