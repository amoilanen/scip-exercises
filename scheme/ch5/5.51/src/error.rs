//! Errors carry the message that the driver loop reports on standard error,
//! after a semicolon. Where the C version jumped back to the driver loop
//! with longjmp, the operations here return the error, and ? passes it up.

use crate::machine::Machine;
use crate::object::Value;

#[derive(Debug)]
pub struct Error {
    pub message: String,
}

pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    /// An error without irritants, such as running out of memory.
    pub fn abort(message: &str) -> Error {
        Error {
            message: message.to_owned(),
        }
    }
}

impl Machine {
    /// An error whose message is followed by the printed irritant. The
    /// irritant is printed now, while it is still valid.
    pub fn error(&self, message: &str, irritant: Value) -> Error {
        Error {
            message: format!("{message} {}", self.write_to_string(irritant)),
        }
    }

    /// An error whose message is followed by each of the list of irritants.
    pub fn error_list(&self, message: &str, irritants: Value) -> Error {
        let mut text = message.to_owned();
        let mut list = irritants;
        while list.is_pair() {
            text.push(' ');
            text.push_str(&self.write_to_string(self.cell_car(list)));
            list = self.cell_cdr(list);
        }
        Error { message: text }
    }
}
