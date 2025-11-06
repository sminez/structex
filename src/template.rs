//! Simple template parsing and rendering
use crate::{
    parse::{self, ParseInput},
    re::{Re, Writable},
    se::TaggedCaptures,
};
use std::{
    fmt,
    io::{self, Write},
};

pub type Error = parse::Error<ErrorKind>;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Template<'a> {
    raw: &'a str,
    fragments: Vec<Fragment>,
}

impl<'a> Template<'a> {
    pub fn parse(template: &'a str) -> Result<Self, Error> {
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
            raw: template,
            fragments,
        })
    }

    pub fn render_to<R, W>(&self, w: &mut W, caps: &TaggedCaptures<'_, R>) -> io::Result<usize>
    where
        R: Re,
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

    pub fn render<R>(&self, caps: &TaggedCaptures<'_, R>) -> io::Result<String>
    where
        R: Re,
    {
        let mut buf = Vec::with_capacity(self.raw.len() * 2);
        self.render_to(&mut buf, caps)?;

        Ok(String::from_utf8(buf).unwrap())
    }
}

#[cfg(test)]
mod tests {
    use crate::Captures;

    use super::*;
    use simple_test_case::test_case;

    #[derive(Debug, PartialEq, Eq)]
    enum Tag<'a> {
        Cap(usize),
        Lit(&'a str),
        Esc(char),
    }

    use Tag::*;

    fn tags<'a>(t: &Template<'a>) -> Vec<Tag<'a>> {
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
        let caps: TaggedCaptures<'_, regex::Regex> = TaggedCaptures {
            captures: Captures::new("foo bar", vec![Some((0, 7)), Some((0, 3)), Some((4, 7))]),
            action: None,
        };

        let t = Template::parse(s).unwrap();
        let rendered = t.render(&caps).unwrap();

        assert_eq!(rendered, expected);
    }
}
