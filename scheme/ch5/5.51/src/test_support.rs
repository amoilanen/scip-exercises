use std::io::Cursor;

use crate::machine::{Error, Machine};
use crate::{driver_loop, reader::Reader};

pub fn reader(source: &str) -> Reader {
    Reader::new(Cursor::new(source.to_owned()))
}

/// The lines that the driver loop prints for the program, with errors after
/// a semicolon.
pub fn run(program: &str) -> Vec<String> {
    let mut lines = Vec::new();
    driver_loop(&mut reader(program), &mut Machine::new(), |output| {
        lines.push(match output {
            Ok(text) => text,
            Err(Error(message)) => format!(";{message}"),
        })
    });
    lines
}
