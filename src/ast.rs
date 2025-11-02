//! Ast parsing for structural regular expressions.
use crate::parse::{ParseInput, Span};
use std::fmt;

/// An error that can arise during expression parsing.
pub type ParseError = crate::parse::Error<ErrorKind>;

/// A list specifying the different categories of errors that can be encountered while parsing a
/// structural regular expression.
///
/// It is used with the [ParseError] type.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    /// A parallel group was provided without any branches.
    EmptyGroup,
    /// A branch in a parallel group contained no expressions.
    EmptyGroupBranch,
    /// The provided expression was empty.
    EmptyExpression,
    /// The provided top level expression was a single action.
    /// Such an expression would only ever run the action once for all inputs.
    TopLevelAction,
    /// An expected delimiter was not found.
    ///
    /// Typically this is encountered when an parallel group or delimited string is missing its
    /// closing delimiter.
    MissingDelimiter(char),
    /// Multiple distinct expressions were present in the provided input.
    ///
    /// To run multiple expressions over a haystack you should provide a top level parallel group.
    MultipleBranches,
    /// An unexpected character was encountered in the input expression.
    UnexpectedChar(char),
    /// End of file was encountered before a full expression could be parsed.
    UnexpectedEof,
    /// An action tag was found that did not match the allowed single argument tags specified as
    /// part of a [StructexBuilder][crate::StructexBuilder].
    UnknownArgumentTag(char),
    /// An action tag was found that did not match the allowed zero argument tags specified as
    /// part of a [StructexBuilder][crate::StructexBuilder].
    UnknownTag(char),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyGroup => write!(f, "empty group"),
            Self::EmptyGroupBranch => write!(f, "empty group branch"),
            Self::EmptyExpression => write!(f, "empty program"),
            Self::MissingDelimiter(ch) => write!(f, "missing delimiter '{ch}'"),
            Self::MultipleBranches => write!(f, "multiple branches when one was expected"),
            Self::TopLevelAction => write!(f, "top level expression can not be an action"),
            Self::UnexpectedChar(ch) => write!(f, "unexpected character in input '{ch}'"),
            Self::UnexpectedEof => write!(f, "unexpected EOF"),
            Self::UnknownArgumentTag(ch) => {
                write!(f, "{ch} is not a registered single argument tag")
            }
            Self::UnknownTag(ch) => write!(f, "{ch} is not a registered bare tag"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum Ast {
    /// If the current dot narrows to the given regex, replace dot and run the given nodes in
    /// series.
    Narrow(ReNode),

    /// Extract all non-overlapping matches of a regex from dot and
    /// then run the the provided nodes over each match.
    Extract(ReNode),
    /// Extract all non-overlapping matches of a regex from dot and
    /// then run the the provided nodes over the regions between
    /// each match.
    ExtractBetween(ReNode),

    /// If the current dot matches the given regex run the given
    /// nodes in parallel over dot.
    Guard(ReNode),
    /// If the current dot doesn't match the given regex run the given
    /// nodes in parallel over dot.
    InvGuard(ReNode),

    /// Run each node in parallel over the current dot.
    Parallel(Sequence),

    /// An action to be emitted alongside the corresponding match.
    Action(Action),

    /// Comments are captured to correctly track spans but are
    /// dropped when compiling
    Comment(Span),
}

impl Ast {
    fn is_comment(&self) -> bool {
        matches!(self, Ast::Comment(_))
    }

    pub fn span(&self) -> &Span {
        match self {
            Ast::Narrow(n)
            | Ast::Extract(n)
            | Ast::ExtractBetween(n)
            | Ast::Guard(n)
            | Ast::InvGuard(n) => &n.span,
            Ast::Parallel(s) => &s.span,
            Ast::Action(a) => &a.span,
            Ast::Comment(s) => s,
        }
    }
}

// Spanned Ast nodes

#[derive(Debug, PartialEq, Eq)]
pub(super) struct ReNode {
    pub span: Span,
    pub re: String,
    pub node: Box<Ast>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Sequence {
    pub span: Span,
    pub nodes: Vec<Ast>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Action {
    pub span: Span,
    pub tag: Option<char>,
    pub s: Option<String>,
}

#[derive(Debug)]
pub(super) struct Parser<'i, 'c> {
    input: ParseInput<'i>,
    allowed_argless_tags: Option<&'c str>,
    allowed_single_arg_tags: Option<&'c str>,
}

impl<'i, 'c> Parser<'i, 'c> {
    pub fn new(prog: &'i str) -> Self {
        Self {
            input: ParseInput::new(prog),
            allowed_argless_tags: None,
            allowed_single_arg_tags: None,
        }
    }

    pub fn with_allowed_argless_tags(mut self, allowed: Option<&'c str>) -> Self {
        self.allowed_argless_tags = allowed;
        self
    }

    pub fn with_allowed_single_arg_tags(mut self, allowed: Option<&'c str>) -> Self {
        self.allowed_single_arg_tags = allowed;
        self
    }

    pub fn parse(&self) -> Result<Ast, ParseError> {
        self.reset();
        let mut seq = Sequence {
            span: self.input.span(),
            nodes: vec![],
        };

        while !self.input.at_eof() {
            let node = self.parse1(false)?;
            if !node.is_comment() {
                seq.nodes.push(node);
            }
            self.input.consume_whitespace();
        }

        match seq.nodes.len() {
            0 => Err(self.error(ErrorKind::EmptyExpression)),
            1 => match seq.nodes.remove(0) {
                Ast::Action(_) => Err(self.error(ErrorKind::TopLevelAction)),
                Ast::Comment(_) => unreachable!(),
                node => Ok(node),
            },
            _ => Err(self.error(ErrorKind::MultipleBranches)),
        }
    }

    fn parse1(&self, in_group: bool) -> Result<Ast, ParseError> {
        self.input.consume_whitespace();

        while !self.input.at_eof() {
            match self.parse1_inner(in_group)? {
                Ast::Comment(_) => self.input.consume_whitespace(),
                node => return Ok(node),
            }
        }

        Err(self.error(ErrorKind::UnexpectedEof))
    }

    /// Must be called after consuming whitespace and comments
    fn parse1_inner(&self, in_group: bool) -> Result<Ast, ParseError> {
        let ast = match self.input.char() {
            '#' => Ast::Comment(self.parse_comment()),

            // Braced group
            '{' => Ast::Parallel(self.parse_group()?),

            // Narrow
            'n' => Ast::Narrow(self.parse_narrow(in_group)?),

            // Extract
            'x' => Ast::Extract(self.parse_extract(in_group)?),
            'y' => Ast::ExtractBetween(self.parse_extract(in_group)?),

            // Guard
            'g' => Ast::Guard(self.parse_guard(in_group)?),
            'v' => Ast::InvGuard(self.parse_guard(in_group)?),

            'X' => {
                let start = self.input.pos();
                self.input.advance();
                let node = Box::new(self.parse1(in_group)?);

                Ast::Extract(ReNode {
                    span: self.span().with_start(start),
                    re: ".*\n".to_string(),
                    node,
                })
            }

            ';' => return Err(self.error(ErrorKind::UnexpectedChar(';'))),
            '}' => return Err(self.error(ErrorKind::UnexpectedChar('}'))),

            // Default to trying to parse an action
            _ => Ast::Action(self.parse_action()?),
        };

        Ok(ast)
    }

    fn reset(&self) {
        self.input.reset();
    }

    /// Crate a null span at the current parser position
    fn span(&self) -> Span {
        self.input.span()
    }

    fn error(&self, kind: ErrorKind) -> ParseError {
        ParseError::new(kind, self.input.text(), self.input.span_char())
    }

    fn emit_match(&self) -> Ast {
        Ast::Action(Action {
            span: self.span(),
            tag: None,
            s: None,
        })
    }

    fn parse1_or_emit_match(&self, in_group: bool) -> Result<Ast, ParseError> {
        if self.input.at_eof() {
            return Ok(self.emit_match());
        }

        match self.parse1(in_group) {
            Ok(node) => Ok(node),
            Err(e) if e.kind == ErrorKind::UnexpectedEof => Ok(self.emit_match()),
            Err(e) if in_group && e.kind == ErrorKind::UnexpectedChar(';') => Ok(self.emit_match()),
            Err(e) => Err(e),
        }
    }

    fn parse_comment(&self) -> Span {
        assert_eq!(self.input.char(), '#');
        let span = self.input.span();
        let mut end = self.input.pos();

        loop {
            if self.input.try_consume("#") {
                self.input.consume_until('\n');
                end = self.input.pos();
                self.input.consume_whitespace();
            } else {
                break;
            }
        }

        span.with_end(end)
    }

    fn parse_action(&self) -> Result<Action, ParseError> {
        let start = self.input.pos();
        let tag = self.input.char();
        self.input.advance();

        let s = if self.input.try_char() == Some('/') {
            if let Some(allowed) = self.allowed_single_arg_tags.as_ref()
                && !allowed.contains(tag)
            {
                return Err(self.error(ErrorKind::UnknownArgumentTag(tag)));
            }

            Some(self.parse_delimited_str()?)
        } else {
            if let Some(allowed) = self.allowed_argless_tags.as_ref()
                && !allowed.contains(tag)
            {
                return Err(self.error(ErrorKind::UnknownTag(tag)));
            }

            None
        };

        Ok(Action {
            span: self.span().with_start(start),
            tag: Some(tag),
            s,
        })
    }

    fn parse_narrow(&self, in_group: bool) -> Result<ReNode, ParseError> {
        assert_eq!(self.input.char(), 'n');

        let start = self.input.pos();
        self.input.advance();
        let re = self.parse_delimited_str()?;
        let node = Box::new(self.parse1_or_emit_match(in_group)?);
        let end = node.span().end;

        Ok(ReNode {
            span: Span::new(start, end),
            re,
            node,
        })
    }

    fn parse_extract(&self, in_group: bool) -> Result<ReNode, ParseError> {
        assert!("xy".contains(self.input.char()));

        let start = self.input.pos();
        self.input.advance();
        let re = self.parse_delimited_str()?;
        let node = Box::new(self.parse1_or_emit_match(in_group)?);
        let end = node.span().end;

        Ok(ReNode {
            span: Span::new(start, end),
            re,
            node,
        })
    }

    fn parse_guard(&self, in_group: bool) -> Result<ReNode, ParseError> {
        assert!("gv".contains(self.input.char()));

        let start = self.input.pos();
        self.input.advance();
        let re = self.parse_delimited_str()?;
        let node = Box::new(self.parse1_or_emit_match(in_group)?);
        let end = node.span().end;

        Ok(ReNode {
            span: Span::new(start, end),
            re,
            node,
        })
    }

    fn parse_group(&self) -> Result<Sequence, ParseError> {
        assert_eq!(self.input.char(), '{');
        let mut seq = Sequence {
            span: self.input.span(),
            nodes: vec![],
        };
        let mut branch = Vec::new();
        self.input.advance();

        loop {
            self.input.consume_whitespace();
            match self.input.try_char() {
                Some('}') => {
                    self.input.advance(); // consume the '}'

                    return if seq.nodes.is_empty() {
                        Err(self.error(ErrorKind::EmptyGroup))
                    } else {
                        seq.span = seq.span.with_end(self.input.pos());
                        Ok(seq)
                    };
                }
                Some(_) => (),
                None => return Err(self.error(ErrorKind::MissingDelimiter('}'))),
            }

            let node = self.parse1(true)?;
            if !node.is_comment() {
                branch.push(node);
            }

            self.input.consume_whitespace();
            if self.input.char() == ';' {
                self.input.advance(); // consume the ';'

                let node = match branch.len() {
                    0 => return Err(self.error(ErrorKind::EmptyGroupBranch)),
                    1 => branch.remove(0),
                    _ => return Err(self.error(ErrorKind::MultipleBranches)),
                };

                seq.nodes.push(node);
                branch = Vec::new();
            }
        }
    }

    /// Can't use ParseInput::read_until for this (or return &str) as we need to handle
    /// escaping the delimiter inside of the string.
    fn parse_delimited_str(&self) -> Result<String, ParseError> {
        assert_eq!(self.input.char(), '/');
        if !self.input.advance() {
            return Err(self.error(ErrorKind::UnexpectedEof));
        }

        let mut s = String::new();
        let mut prev = '/';

        while !self.input.at_eof() {
            let ch = self.input.char();
            if ch == '/' {
                if prev == '\\' {
                    s.push(ch);
                } else {
                    self.input.advance(); // consume the delimiter
                    return Ok(s);
                }
            } else {
                if prev == '\\' {
                    s.push('\\');
                }
                if ch != '\\' {
                    s.push(ch);
                }
            }
            prev = ch;
            self.input.advance();
        }

        Err(self.error(ErrorKind::MissingDelimiter('/')))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Position;
    use simple_test_case::test_case;

    fn span(start: (usize, usize, usize), end: (usize, usize, usize)) -> Span {
        Span::new(
            Position::new(start.0, start.1, start.2),
            Position::new(end.0, end.1, end.2),
        )
    }

    fn action(tag: char, span: Span) -> Ast {
        Ast::Action(Action {
            span,
            tag: Some(tag),
            s: None,
        })
    }

    fn template_action(tag: char, s: &str, span: Span) -> Ast {
        Ast::Action(Action {
            span,
            tag: Some(tag),
            s: Some(s.to_string()),
        })
    }

    #[test]
    fn parse_action_returns_correct_span() {
        let p = Parser::new("c/bar/  # comment");
        let action = p.parse_action().unwrap();

        assert_eq!(action.tag, Some('c'));

        let substr = p.input.span_text(&action.span);
        assert_eq!(substr, "c/bar/");
    }

    #[test]
    fn parse_action_returns_correct_span_without_a_template() {
        let p = Parser::new("a # comment");
        let action = p.parse_action().unwrap();

        assert_eq!(action.tag, Some('a'));

        let substr = p.input.span_text(&action.span);
        assert_eq!(substr, "a");
    }

    #[test]
    fn parse_extract_returns_correct_span() {
        let p = Parser::new("x/foo/ a/baz/  # comment");
        let extract = p.parse_extract(false).unwrap();
        let substr = p.input.span_text(&extract.span);

        assert_eq!(substr, "x/foo/ a/baz/");
    }

    #[test]
    fn parse_guard_returns_correct_span() {
        let p = Parser::new("g/foo/ c/bar/   ");
        let guard = p.parse_guard(false).unwrap();
        let substr = p.input.span_text(&guard.span);

        assert_eq!(substr, "g/foo/ c/bar/");
    }

    #[test]
    fn parse_group_returns_correct_span() {
        let p = Parser::new("  { x/foo/ c/bar/;\nx/bar/ c/foo/; }    ");
        p.input.consume_whitespace();
        let seq = p.parse_group().unwrap();
        let substr = p.input.span_text(&seq.span);

        assert_eq!(substr, "{ x/foo/ c/bar/;\nx/bar/ c/foo/; }");
    }

    #[test_case("n/foo/ a",
        Ast::Narrow(ReNode {
            span: span((0, 1, 1), (8, 1, 9)),
            re: "foo".to_string(),
            node: Box::new(action('a', span((7, 1, 8), (8, 1, 9)))),
        });
        "narrow"
    )]
    #[test_case("x/foo/ a",
        Ast::Extract(ReNode {
            span: span((0, 1, 1), (8, 1, 9)),
            re: "foo".to_string(),
            node: Box::new(action('a', span((7, 1, 8), (8, 1, 9)))),
        });
        "extract"
    )]
    #[test_case("y/foo/ a",
        Ast::ExtractBetween(ReNode {
            span: span((0, 1, 1), (8, 1, 9)),
            re: "foo".to_string(),
            node: Box::new(action('a', span((7, 1, 8), (8, 1, 9)))),
        });
        "extract between"
    )]
    #[test_case("X a",
        Ast::Extract(ReNode {
            span: span((0, 1, 1), (3, 1, 4)),
            re: ".*\n".to_string(),
            node: Box::new(action('a', span((2, 1, 3), (3, 1, 4)))),
        });
        "extract lines"
    )]
    #[test_case("g/foo/ a",
        Ast::Guard(ReNode {
            span: span((0, 1, 1), (8, 1, 9)),
            re: "foo".to_string(),
            node: Box::new(action('a', span((7, 1, 8), (8, 1, 9)))),
        });
        "guard"
    )]
    #[test_case("v/foo/ a",
        Ast::InvGuard(ReNode {
            span: span((0, 1, 1), (8, 1, 9)),
            re: "foo".to_string(),
            node: Box::new(action('a', span((7, 1, 8), (8, 1, 9)))),
        });
        "inv-guard"
    )]
    #[test_case("{ a; b/foo/; }",
        Ast::Parallel(Sequence {
            span: span((0, 1, 1), (14, 1, 15)),
            nodes: vec![
                action('a', span((2, 1, 3), (3, 1, 4))),
                template_action('b', "foo", span((5, 1, 6), (11, 1, 12))),
            ],
        });
        "group"
    )]
    #[test]
    fn parse1_works(input: &str, expected: Ast) {
        let p = Parser::new(input);
        let ast = p.parse1(false).unwrap();
        assert_eq!(ast, expected);
    }
}
