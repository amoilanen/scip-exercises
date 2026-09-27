//! Exercise 5.51: a Scheme interpreter, the explicit-control evaluator of
//! section 5.4, in Rust.
//!
//! The driver loop reads the expressions of a program from a file or from
//! standard input, evaluates them and prints their values. After an error
//! it goes on with the next expression, and exits with status 1 at the end.

mod environment;
mod error;
mod eval;
mod machine;
mod memory;
mod object;
mod primitives;
mod printer;
mod reader;

use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::process::ExitCode;

use error::{Error, Result};
use machine::Machine;
use object::Value;

/// Reads, evaluates and prints one expression. Returns false at the end of
/// the input.
fn read_eval_print(machine: &mut Machine, input: &mut dyn BufRead) -> Result<bool> {
    let exp = machine.read(input)?;
    if exp == Value::Eof {
        return Ok(false);
    }
    let val = machine.evaluate(exp, machine.global_env)?;
    if val != Value::Unspecified {
        let text = machine.write_to_string(val);
        machine.output(&text);
        machine.output("\n");
    }
    Ok(true)
}

/// Reports an error after what the program has printed so far.
fn report(error: &Error) {
    let _ = io::stdout().flush();
    let _ = writeln!(io::stderr(), ";{}", error.message);
}

/// Returns whether every expression was evaluated without error.
fn driver_loop(machine: &mut Machine, input: &mut dyn BufRead) -> bool {
    let mut failed = false;
    loop {
        match read_eval_print(machine, input) {
            Ok(true) => {}
            Ok(false) => return !failed,
            Err(error) => {
                failed = true;
                report(&error);
                machine.reset_evaluator();
            }
        }
    }
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        eprintln!("usage: {} [file]", args[0]);
        return ExitCode::FAILURE;
    }
    let mut input: Box<dyn BufRead> = match args.get(1) {
        Some(path) => match File::open(path) {
            Ok(file) => Box::new(BufReader::new(file)),
            Err(error) => {
                eprintln!("{path}: {error}");
                return ExitCode::FAILURE;
            }
        },
        None => Box::new(io::stdin().lock()),
    };

    let mut machine = match Machine::new() {
        Ok(machine) => machine,
        Err(error) => {
            report(&error);
            return ExitCode::FAILURE;
        }
    };
    let succeeded = driver_loop(&mut machine, &mut *input);
    let _ = io::stdout().flush();
    if succeeded {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
