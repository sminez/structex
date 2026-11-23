use crate::re::{Haystack, RawCaptures, RegexEngine};
use regex_cursor::{Input, engines::meta, regex_automata::util::syntax};
use std::{error, fmt, io::Read};

mod caching_stream;

pub use caching_stream::{CachingStream, StreamSlice};

/// A [Structex][crate::Structex] powered by an underlying [regex_cursor::engines::meta] engine.
pub type Structex = crate::Structex<Regex>;

/// A [RegexEngine] backed by [regex_cursor].
#[derive(Debug, Clone)]
pub struct Regex {
    inner: meta::Regex,
    /// Whether every match of this regex must end at the end of the input. regex-cursor fixes the
    /// end of the input when a search starts, so for a stream we need to read to EOF before
    /// running a search for these patterns.
    end_anchored: bool,
}

/// An error compiling a regular expression for use with [Regex].
#[derive(Debug)]
pub struct CompileError(Box<dyn error::Error + Send + Sync>);

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl error::Error for CompileError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.0.source()
    }
}

impl RegexEngine for Regex {
    type CompileError = CompileError;

    fn compile(pattern: &str) -> Result<Self, Self::CompileError> {
        let config = syntax::Config::new().multi_line(true);
        let re = meta::Regex::builder()
            .syntax(config)
            .build(pattern)
            .map_err(|e| CompileError(Box::new(e)))?;
        let hir = syntax::parse_with(pattern, &config)
            .expect("pattern was successfully compiled with the same config");

        // This is a bit of a hack, but otherwise we need to add regex_syntax as a dependency
        // just to be able to check if look_set_suffix contains a Look::End.
        let end_anchored = hir
            .properties()
            .look_set_suffix()
            .iter()
            .any(|look| look.as_char() == 'z');

        Ok(Self {
            inner: re,
            end_anchored,
        })
    }
}

impl Haystack<Regex> for str {
    fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
        let mut input = Input::new(self);
        input.set_span(from..to);

        re.inner.is_match(input)
    }

    fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
        let mut input = Input::new(self);
        input.set_span(from..to);

        let mut caps = re.inner.create_captures();
        re.inner.captures(input, &mut caps);

        caps.is_match()
            .then(|| RawCaptures::new(caps.iter().map(|span| span.map(|s| (s.start, s.end)))))
    }
}

impl<R> Haystack<Regex> for CachingStream<R>
where
    R: Read,
{
    fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
        self.with_input(from, to, re.end_anchored, |input| re.inner.is_match(input))
    }

    fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
        self.with_input(from, to, re.end_anchored, |input| {
            let mut caps = re.inner.create_captures();
            re.inner.captures(input, &mut caps);

            caps.is_match()
                .then(|| RawCaptures::new(caps.iter().map(|span| span.map(|s| (s.start, s.end)))))
        })
    }
}
