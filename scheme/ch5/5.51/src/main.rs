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
    let mut reader = Reader::new(input);
    let mut machine = Machine::new();
    let mut failed = false;
    loop {
        match read_eval_print(&mut reader, &mut machine) {
            Ok(true) => {}
            Ok(false) => break,
            Err(Error(message)) => {
                failed = true;
                io::stdout().flush().ok();
                eprintln!(";{message}");
                machine.reset();
            }
        }
    }
    if failed {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

/// Returns false at the end of the input.
fn read_eval_print(reader: &mut Reader, machine: &mut Machine) -> Result<bool> {
    let Some(exp) = reader.read(machine)? else {
        return Ok(false);
    };
    let val = machine.evaluate(exp)?;
    if val != Value::Unspecified {
        println!("{}", machine.show(val, true));
    }
    Ok(true)
}
