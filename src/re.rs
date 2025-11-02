//! The required interface for an underlying regex engine

/// An [Re] is an underlying regular expression engine that can be used to match and extract text
/// as part of a structural regular expression.
///
/// The interface provided by this trait is intended for use by [Structex][crate::Structex] as an
/// internal implementation detail and is likely not particularly useful outside of this crate.
/// Implementors of this trait should pay particular attention to the requirements around
/// compilation in order for the resulting regex expressions to be compatible with this crate.
pub trait Re: Sized {
    /// Error type that is returned by the [Re::compile] method when compilation of a given regular
    /// expression fails. This will be wrapped into an [Error][crate::Error] when returned from
    /// [Structex::compile][crate::Structex::new] or [StructexBuilder::build][crate::StructexBuilder::build].
    type CompileError: std::error::Error + 'static;

    /// Attempt to compile the given regular expression for use inside of a [Structex][crate::Structex].
    ///
    /// ### Multi-line matching
    ///
    /// Almost all use cases for structural regular expressions involve matching patterns that span
    /// multiple lines. As such, any underlying regular expression engine used with
    /// [Structex][crate::Structex] must support multi-line patterns in order to provide the
    /// expected semantics of each of the different operators that build upon it.
    ///
    /// See [here][0] for information on how this is implemented in the `regex` crate for
    /// reference.
    ///
    /// [0]: https://docs.rs/regex/latest/regex/struct.RegexBuilder.html#method.multi_line
    fn compile(re: &str) -> Result<Self, Self::CompileError>;

    /// Returns true if there is a match for the regex anywhere in the given haystack.
    ///
    /// This does not need to search for the leftmost-longest match and where possible should be
    /// faster to run that [Re::captures] which needs to extract the position of the match itself
    /// and all submatches.
    fn is_match(&self, haystack: &str) -> bool;

    /// Searches for the first match of this regex in the given haystack, returning the overall
    /// match along with the matches of each capture group in the regex. If no match is found, then
    /// None is returned.
    ///
    /// See [Captures::new] for requirements around constructing the return type.
    fn captures(&self, haystack: &str) -> Option<Captures>;

    /// This method provides the same semantics as [Re::is_match] but restricts the match to be
    /// between the provided byte offsets within the given haystack.
    fn matches_between(&self, haystack: &str, from: usize, to: usize) -> bool {
        self.is_match(&haystack[from..to])
    }

    /// This method provides the same semantics as [Re::captures] but restricts the match to be
    /// between the provided byte offsets within the given haystack, updating the returned captures
    /// to provide the correct offsets for each match and submatch position.
    fn captures_between(&self, haystack: &str, from: usize, to: usize) -> Option<Captures> {
        let mut caps = self.captures(&haystack[from..to])?;
        caps.apply_offset(from);

        Some(caps)
    }
}

/// Represents the capture group positions for a single [Re] match in terms of byte offsets into
/// the original haystack that the match was run against.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Captures {
    caps: Vec<Option<(usize, usize)>>,
}

impl Captures {
    /// # Panics
    /// Caps must return at least one item for the full match.
    pub fn new(caps: impl Iterator<Item = Option<(usize, usize)>>) -> Self {
        let caps: Vec<_> = caps.collect();
        assert!(!caps.is_empty(), "empty captures");

        Self { caps }
    }

    fn apply_offset(&mut self, i: usize) {
        for cap in self.caps.iter_mut().flatten() {
            cap.0 += i;
            cap.1 += i;
        }
    }

    pub fn from(&self) -> usize {
        self.get_match().0
    }

    pub fn to(&self) -> usize {
        self.get_match().1
    }

    pub fn get_match(&self) -> (usize, usize) {
        self.caps[0].unwrap()
    }

    pub fn get(&self, n: usize) -> Option<(usize, usize)> {
        self.caps.get(n).copied()?
    }

    pub fn match_text<'h>(&self, haystack: &'h str) -> &'h str {
        let (from, to) = self.get_match();

        &haystack[from..to]
    }

    pub fn submatch_text<'h>(&self, haystack: &'h str, n: usize) -> Option<&'h str> {
        let (from, to) = self.get(n)?;

        Some(&haystack[from..to])
    }

    pub fn iter_caps<'h>(&self, haystack: &'h str) -> impl Iterator<Item = Option<&'h str>> {
        self.caps
            .iter()
            .map(|cap| cap.map(|(from, to)| &haystack[from..to]))
    }
}

#[cfg(feature = "regex")]
impl Re for regex::Regex {
    type CompileError = regex::Error;

    fn compile(re: &str) -> Result<Self, Self::CompileError> {
        regex::RegexBuilder::new(re).multi_line(true).build()
    }

    fn is_match(&self, haystack: &str) -> bool {
        self.is_match(haystack)
    }

    fn captures(&self, haystack: &str) -> Option<Captures> {
        let caps = self.captures(haystack)?;

        Some(Captures::new(
            caps.iter()
                .map(|cap| cap.map(|cap| (cap.start(), cap.end()))),
        ))
    }
}
