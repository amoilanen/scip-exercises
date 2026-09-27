//! Exercise 5.51: the explicit-control evaluator of section 5.4 in Rust.
//!
//! Reads a program from the file given as the argument or from standard
//! input and prints the value of each expression. After an error it goes on
//! with the next expression and in the end exits with status 1.

mod environment;
mod eval;
mod machine;
mod primitives;
mod printer;
mod reader;

use std::io::{self, BufRead, BufReader, Write};
use std::process::ExitCode;
use std::{env, fs::File};

use machine::{Error, Machine, Result, Value};
use reader::Reader;

fn main() -> ExitCode {
    let input: Box<dyn BufRead> = match env::args().nth(1) {
        None => Box::new(io::stdin().lock()),
        Some(path) => match File::open(&path) {
            Ok(file) => Box::new(BufReader::new(file)),
            Err(error) => {
                eprintln!("{path}: {error}");
                return ExitCode::FAILURE;
            }
        },
    };
    let mut failed = false;
    driver_loop(&mut Reader::new(input), &mut Machine::new(), |output| match output {
        Ok(text) => println!("{text}"),
        Err(Error(message)) => {
            failed = true;
            io::stdout().flush().ok();
            eprintln!(";{message}");
        }
    });
    if failed {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

/// Evaluates each expression read and prints its value, unless it is
/// unspecified, or the error that stopped it.
fn driver_loop(reader: &mut Reader, machine: &mut Machine, mut print: impl FnMut(Result<String>)) {
    loop {
        let value = match reader.read(machine) {
            Ok(None) => return,
            Ok(Some(exp)) => machine.evaluate(exp),
            Err(error) => Err(error),
        };
        match value {
            Ok(Value::Unspecified) => {}
            Ok(value) => print(Ok(machine.show(value, true))),
            Err(error) => {
                machine.reset();
                print(Err(error));
            }
        }
    }
}
