use std::{error, fmt};

mod ast;
mod compile;
mod parse;
mod re;
mod se;

pub use ast::{ErrorKind, ParseError};
pub use re::{Captures, Re};
pub use se::{Action, Match, MatchIter, Structex, StructexBuilder};

/// An error that occurred during parsing or compiling a strutural regular expression.
#[derive(Debug)]
pub enum Error {
    /// A syntax error in the provided structural regular expression.
    Syntax(ParseError),
    /// An error that occurred during parsing or compiling a regular expression with the given [Re]
    /// implementation.
    Regex(Box<dyn error::Error>),
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Syntax(e) => write!(f, "{e}"),
            Self::Regex(e) => write!(f, "{e}"),
        }
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Error::Syntax(err)
    }
}
