#![doc = include_str!("../README.md")]
use std::{error, fmt};

mod ast;
mod compile;
mod parse;
pub mod re;
mod se;
pub mod template;

pub use ast::{ErrorKind, ParseError};
pub use re::Captures;
pub use se::{Action, Structex, StructexBuilder, TaggedCaptures, TaggedCapturesIter};

/// An error that occurred during parsing or compiling a strutural regular expression.
#[derive(Debug)]
pub enum Error {
    /// A syntax error in the provided structural regular expression.
    Syntax(ParseError),
    /// An error that occurred during parsing or compiling a regular expression with the given
    /// [RegexEngine][re::RegexEngine].
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
