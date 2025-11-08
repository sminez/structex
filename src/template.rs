//! Simple template parsing and rendering for use in actions.
//!
//! This module provides a simple [Template] type that can be parsed from the argument string of a
//! given [Action][crate::Action] and used for rendering string content based on submatches
//! extracted by a [Structex][crate::Structex].
//!
//! # Syntax
//! The syntax supported for templating is extremely minimal and focuses solely on injecting
//! submatches into a user provided template. In its simplest form a template is simply a string
//! literal, but the syntax also supports referencing submatches by their index. To inject the
//! contents of a submatch at a particular point within a template, place the capture index inside
//! of curly braces: `"submatch 1 is {1} and submatch 2 is {2}"`.
//!
//! ## Syntax Errors
//! It is an error to have an unclosed `{}` pair within a template or to place something other than
//! a capture index inside of curly braces. To escape a curly brace, place a `\` before the opening
//! brace. Closing braces do not need to be escaped.
//!
//! ```
//! use structex::template::Template;
//!
//! // The following are all valid templates
//!
//! // String literals with no references are allowed
//! assert!(Template::parse("hello, world!").is_ok());
//!
//! // Raw strings with `\n` and `\t` escape sequences
//! assert!(Template::parse(r#"hello,\tworld!\n"#).is_ok());
//!
//! // Escaping `{` requires `\\` to provide a literal backslash
//! assert!(Template::parse("\\{ 1, 2, 3 }").is_ok());
//!
//! // Using a raw string is easier
//! assert!(Template::parse(r#"\{ "hello", "world!" }"#).is_ok());
//!
//! // A single reference
//! assert!(Template::parse("hello, {1}!").is_ok());
//!
//! // Multiple references
//! assert!(Template::parse("{2}, {1}!").is_ok());
//!
//! // The same reference multiple times
//! assert!(Template::parse("{1}, {1}!").is_ok());
//!
//!
//! // The following are all invalid templates
//!
//! // An unclosed capture reference
//! assert!(Template::parse("{1").is_err());
//!
//! // An unknown escape sequence
//! assert!(Template::parse("\\[").is_err());
//!
//! // Something other than a capture index as a reference
//! assert!(Template::parse("{foo}").is_err());
//! ```
//!
//! # Usage
//! To make use of [Templates][Template] with the rest of this crate, construct your templates from
//! the parsed [Actions][crate::Action] returned by a [Structex][crate::Structex] instance and then
//! perform your rendering based on the matches that are yielded from
//! [Structex::iter_tagged_captures][crate::Structex::iter_tagged_captures].
//!
//! ```
//! use structex::{Structex, template::Template};
//! use regex::Regex;
//!
//! let haystack = r#"This is a multi-line
//! string that mentions peoples names.
//! People like Alice and Bob. People
//! like Claire and David, but really
//! we're here to talk about Alice.
//! Alice is everyone's friend."#;
//!
//! let se: Structex<Regex> = Structex::new(r#"
//!   x/(.|\n)*?\./ {
//!     g/Alice/ n/(\w+)\./ p/The last word is '{1}'/;
//!     v/Alice/ n/(\w+)/   p/The first word is '{1}'/;
//!   }
//! "#).unwrap();
//!
//! // Parse and register the templates
//! let templates: Vec<Template> = se
//!     .actions()
//!     .iter()
//!     .map(|action| Template::parse(action.arg().unwrap()).unwrap())
//!     .collect();
//!
//! let output: Vec<String> = se
//!     .iter_tagged_captures(haystack)
//!     .map(|caps| {
//!         let id = caps.id().unwrap();
//!         templates[id].render(&caps).unwrap()
//!     })
//!     .collect();
//!
//! assert_eq!(
//!     &output,
//!     &[
//!         "The first word is 'This'",
//!         "The last word is 'Bob'",
//!         "The last word is 'Alice'",
//!         "The last word is 'friend'",
//!     ]
//! );
//! ```
use crate::{
    parse::{self, ParseInput},
    re::{Sliceable, Writable},
    se::TaggedCaptures,
};
use std::{
    fmt,
    io::{self, Write},
    sync::Arc,
};

/// An error that can arise during template parsing.
pub type Error = parse::Error<ErrorKind>;

/// A list specifying the different categories of errors that can be encountered while parsing a
/// template.
///
/// It is used with the [template::Error][Error] type.
#[non_exhaustive]
#[derive(Debug)]
pub enum ErrorKind {
    /// An invalid variable reference.
    InvalidVariable(String),
    /// An expected delimiter was not found.
    MissingDelimiter(char),
    /// End of file was encountered before a full expression could be parsed.
    UnexpectedEof,
    /// An unknown escape sequence was encounetered.
    UnknownEscape(char),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidVariable(s) => write!(f, "expected a capture index, found {s:?}"),
            Self::MissingDelimiter(ch) => write!(f, "missing delimiter '{ch}'"),
            Self::UnknownEscape(ch) => write!(f, "unknown escape sequence '\\{ch}'"),
            Self::UnexpectedEof => write!(f, "unexpected EOF"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Fragment {
    /// A capture group id that should be looked up in the provided match
    Cap(usize),
    /// An escaped character that should be injected in place of the escape sequence
    Esc(char),
    /// Sub-string of the raw template that should be rendered as-is
    Lit(usize, usize),
}

/// A parsed string template that can be rendered for a given [TaggedCaptures].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Template {
    raw: Arc<str>,
    fragments: Vec<Fragment>,
}

impl Template {
    /// Parses a new [Template] from the given string.
    ///
    /// Errors if the template contains unknown escape sequences or an invalid variable reference.
    ///
    /// See the [module][crate::template] documentation for usage and examples of the supported
    /// syntax.
    pub fn parse(template: &str) -> Result<Self, Error> {
        let input = ParseInput::new(template);
        let mut fragments = Vec::new();
        let mut offset = input.offset();

        let error = |kind: ErrorKind| Error::new(kind, input.text(), input.span_char());
        let push_lit = |offset, input: &ParseInput<'_>, fragments: &mut Vec<Fragment>| {
            if offset < input.offset() {
                fragments.push(Fragment::Lit(offset, input.offset()));
            }
        };

        while !input.at_eof() {
            match input.char() {
                // Escape sequence
                '\\' => {
                    push_lit(offset, &input, &mut fragments);
                    input.advance();

                    match input.try_char() {
                        Some('n') => fragments.push(Fragment::Esc('\n')),
                        Some('t') => fragments.push(Fragment::Esc('\t')),
                        Some('{') => fragments.push(Fragment::Esc('{')),
                        Some(ch) => return Err(error(ErrorKind::UnknownEscape(ch))),
                        None => return Err(error(ErrorKind::UnexpectedEof)),
                    }

                    input.advance();
                    offset = input.offset();
                }

                // Capture group
                '{' => {
                    push_lit(offset, &input, &mut fragments);
                    input.advance();

                    match input.read_until('}') {
                        Some(s) => {
                            let i: usize = s
                                .parse()
                                .map_err(|_| error(ErrorKind::InvalidVariable(s.to_string())))?;

                            fragments.push(Fragment::Cap(i));
                        }
                        None => return Err(error(ErrorKind::UnexpectedEof)),
                    }

                    input.advance();
                    offset = input.offset();
                }

                // any other character
                _ => {
                    input.advance();
                }
            }
        }

        push_lit(offset, &input, &mut fragments);

        Ok(Template {
            raw: Arc::from(template),
            fragments,
        })
    }

    /// Render this template directly to a newly created [String] using the given [TaggedCaptures].
    ///
    /// Returns an error if any of the underlying write calls fail.
    ///
    /// To render to an arbitrary writer instead of a string, see the
    /// [render_to][Template::render_to] method.
    pub fn render<H>(&self, caps: &TaggedCaptures<H>) -> io::Result<String>
    where
        H: Sliceable,
    {
        let mut buf = Vec::with_capacity(self.raw.len() * 2);
        self.render_to(&mut buf, caps)?;

        Ok(String::from_utf8(buf).unwrap())
    }

    /// Render this template to the provided writer using the given [TaggedCaptures].
    ///
    /// Returns an error if any of the underlying write calls fail.
    ///
    /// To render directly to a [String], see the [render][Template::render] method.
    pub fn render_to<H, W>(&self, w: &mut W, caps: &TaggedCaptures<H>) -> io::Result<usize>
    where
        H: Sliceable,
        W: Write,
    {
        let mut n = 0;
        for frag in self.fragments.iter() {
            n += match frag {
                Fragment::Lit(from, to) => {
                    w.write_all(&self.raw.as_bytes()[*from..*to])?;
                    to - from
                }
                Fragment::Esc(ch) => w.write(ch.encode_utf8(&mut [0; 1]).as_bytes())?,
                Fragment::Cap(n) => {
                    match caps.submatch_text(*n) {
                        Some(slice) => slice.write_to(w)?,
                        None => 0, // drop unknown capture references as ""
                    }
                }
            };
        }

        Ok(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Captures;
    use simple_test_case::test_case;

    #[derive(Debug, PartialEq, Eq)]
    enum Tag<'a> {
        Cap(usize),
        Lit(&'a str),
        Esc(char),
    }

    use Tag::*;

    fn tags(t: &Template) -> Vec<Tag<'_>> {
        t.fragments
            .iter()
            .map(|f| match f {
                Fragment::Lit(from, to) => Tag::Lit(&t.raw[*from..*to]),
                Fragment::Cap(n) => Tag::Cap(*n),
                Fragment::Esc(ch) => Tag::Esc(*ch),
            })
            .collect()
    }

    #[test_case(
        "just a raw string",
        &[Lit("just a raw string")];
        "raw string"
    )]
    #[test_case(
        "foo\\nbar\\tbaz\\{",
        &[Lit("foo"), Esc('\n'), Lit("bar"), Esc('\t'), Lit("baz"), Esc('{')];
        "escape sequences"
    )]
    #[test_case(
        "foo {1} {2}",
        &[Lit("foo "), Cap(1), Lit(" "), Cap(2)];
        "variable references"
    )]
    #[test]
    fn parse_works(s: &str, expected: &[Tag<'_>]) {
        let t = Template::parse(s).unwrap();
        let tagged_strs = tags(&t);

        assert_eq!(tagged_strs, expected);
    }

    #[test_case("just a raw string", "just a raw string"; "raw string")]
    #[test_case(">{0}<", ">foo bar<"; "full match")]
    #[test_case("{1} {2}", "foo bar"; "both submatches")]
    #[test_case("{2} {1}", "bar foo"; "flipped submatches")]
    #[test_case("{1}\\n{2}", "foo\nbar"; "submatches and newline")]
    #[test]
    fn render_works(s: &str, expected: &str) {
        let caps: TaggedCaptures<&str> = TaggedCaptures {
            captures: Captures::new("foo bar", vec![Some((0, 7)), Some((0, 3)), Some((4, 7))]),
            action: None,
        };

        let t = Template::parse(s).unwrap();
        let rendered = t.render(&caps).unwrap();

        assert_eq!(rendered, expected);
    }
}
