use std::error;

mod ast;
mod compile;
mod parse;
mod re;
mod se;

pub use ast::{ErrorKind, ParseError};
pub use re::Re;
pub use se::Structex;

#[derive(Debug)]
pub enum Error {
    /// Error while parsing the input expression
    Parse(ParseError),
    /// Invalid regex
    InvalidRegex(Box<dyn error::Error>),
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Error::Parse(err)
    }
}
