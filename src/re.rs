//! The required interface for an underlying regex engine
use std::{io, ops::Range};

/// A [RegexEngine] is an underlying regular expression engine that can be used to match and
/// extract text as part of a structural regular expression.
///
/// The interface provided by this trait is intended for use by [Structex][crate::Structex] as an
/// internal implementation detail and is likely not particularly useful outside of this crate.
/// Implementors of this trait should pay particular attention to the requirements around
/// compilation in order for the resulting regex expressions to be compatible with this crate.
pub trait RegexEngine: Sized {
    /// Error type that is returned by the [RegexEngine::compile] method when compilation of a
    /// given regular expression fails. This will be wrapped into an [Error][crate::Error] when
    /// returned from [Structex::compile][crate::Structex::new] or
    /// [StructexBuilder::build][crate::StructexBuilder::build].
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
}

/// A haystack that can be searched by a [RegexEngine].
///
/// Typically this is a [&str] but some engines may support richer types in order to provide
/// searching of streams or discontiguous inputs.
pub trait Haystack<R>: Sliceable
where
    R: RegexEngine,
{
    /// Returns true if there is a match for the regex between the given byte offsets in the haystack.
    ///
    /// This does not need to search for the leftmost-longest match and where possible should be
    /// faster to run that [Haystack::captures_between] which needs to extract the position of the
    /// match itself and all submatches.
    fn is_match_between(&self, re: &R, from: usize, to: usize) -> bool;

    /// Searches for the first match of this regex between the given byte offsets in the given
    /// haystack, returning the overall match along with the matches of each capture group in the
    /// regex. If no match is found, then None is returned.
    ///
    /// See [RawCaptures::new] for requirements around constructing the return type.
    fn captures_between(&self, re: &R, from: usize, to: usize) -> Option<RawCaptures>;
}

/// Something that supports extracting a contiguous sub-section between two bytes offsets.
pub trait Sliceable: Writable + Copy {
    /// The output of the [slice][Sliceable::slice] method.
    type Slice<'h>: Writable
    where
        Self: 'h;

    /// The contiguous sub-section of self that is denoted by the given byte [Range].
    fn slice<'h>(&'h self, range: Range<usize>) -> Self::Slice<'h>;

    /// The maximum length in bytes.
    ///
    /// This value will be used as the upper bound to extract slices when searching for matches. As
    /// such, this value must be a valid end to the `range` argument to the
    /// [slice][Sliceable::slice] method.
    fn max_len(&self) -> usize;
}

/// Something that can be written to to a given [io::Write].
pub trait Writable {
    /// Writes `self` to the given [io::Write], returning the number of bytes written.
    fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
    where
        W: io::Write;
}

impl Sliceable for &str {
    type Slice<'h>
        = &'h str
    where
        Self: 'h;

    fn slice(&self, range: Range<usize>) -> &str {
        &self[range]
    }

    fn max_len(&self) -> usize {
        self.len()
    }
}

impl Writable for &str {
    fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
    where
        W: io::Write,
    {
        w.write_all(self.as_bytes()).map(|_| self.len())
    }
}

impl Sliceable for &[u8] {
    type Slice<'h>
        = &'h [u8]
    where
        Self: 'h;

    fn slice(&self, range: Range<usize>) -> &[u8] {
        &self[range]
    }

    fn max_len(&self) -> usize {
        self.len()
    }
}

impl Writable for &[u8] {
    fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
    where
        W: io::Write,
    {
        w.write_all(self).map(|_| self.len())
    }
}

/// Represents the capture group positions for a single [RegexEngine] match only in terms of byte
/// offsets into the original haystack.
///
/// This is converted into a [Captures] by [TaggedCapturesIter][crate::TaggedCapturesIter] as
/// matches are returned during iteration.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RawCaptures {
    pub(crate) caps: Vec<Option<(usize, usize)>>,
}

impl RawCaptures {
    /// Constructs a new [RawCaptures] from capture group byte offsets.
    ///
    /// # Panics
    /// The provided `caps` iterator must return at least one non-None item for the full match.
    pub fn new(caps: impl Iterator<Item = Option<(usize, usize)>>) -> Self {
        let caps: Vec<_> = caps.collect();
        assert!(!caps.is_empty(), "empty captures");
        assert!(caps[0].is_some(), "full match is None");

        Self { caps }
    }

    pub(crate) fn get_match(&self) -> (usize, usize) {
        self.caps[0].unwrap()
    }

    pub(crate) fn from(&self) -> usize {
        self.get_match().0
    }

    pub(crate) fn to(&self) -> usize {
        self.get_match().1
    }
}

/// Represents the capture group positions for a single [RegexEngine] match in terms of byte
/// offsets into the original haystack that the match was run against.
#[derive(Debug, PartialEq, Eq)]
pub struct Captures<H>
where
    H: Sliceable,
{
    haystack: H,
    caps: Vec<Option<(usize, usize)>>,
}

impl<H> Captures<H>
where
    H: Sliceable,
{
    pub(crate) fn new(haystack: H, caps: Vec<Option<(usize, usize)>>) -> Self {
        Self { haystack, caps }
    }

    /// The byte offset that this match starts at.
    pub fn from(&self) -> usize {
        self.get_match().0
    }

    /// The byte offset that this match ends at.
    pub fn to(&self) -> usize {
        self.get_match().1
    }

    /// The start and end byte offsets of this match.
    pub fn get_match(&self) -> (usize, usize) {
        self.caps[0].unwrap()
    }

    /// The start and end byte offsets of the given submatch.
    ///
    /// Submatch 0 is guaranteed to be `Some`, with [get_match][Captures::get_match] available as a
    /// convenience method to directly access it. All other submatches may return `None` if they
    /// were not present in the matched pattern.
    pub fn get(&self, n: usize) -> Option<(usize, usize)> {
        self.caps.get(n).copied()?
    }

    /// The length of the full match in bytes.
    pub fn len(&self) -> usize {
        let (from, to) = self.get_match();
        to - from
    }

    /// Whether or not this match is equal to the empty string.
    pub fn is_empty(&self) -> bool {
        let (from, to) = self.get_match();
        from == to
    }

    /// The full text of the match in the original haystack.
    pub fn match_text(&self) -> H::Slice<'_> {
        let (from, to) = self.get_match();

        self.haystack.slice(from..to)
    }

    /// The full text of the submatch, if present, in the original haystack.
    pub fn submatch_text(&self, n: usize) -> Option<H::Slice<'_>> {
        let (from, to) = self.get(n)?;

        Some(self.haystack.slice(from..to))
    }

    /// Iterate over all submatches starting with the full match.
    pub fn iter_submatches(&self) -> impl Iterator<Item = Option<H::Slice<'_>>> {
        self.caps
            .iter()
            .map(|cap| cap.map(|(from, to)| self.haystack.slice(from..to)))
    }
}

#[cfg(feature = "regex")]
mod impl_for_regex {
    use super::*;
    use regex::{Error, Regex, RegexBuilder};

    impl RegexEngine for Regex {
        type CompileError = Error;

        fn compile(re: &str) -> Result<Self, Self::CompileError> {
            RegexBuilder::new(re).multi_line(true).build()
        }
    }

    impl Haystack<Regex> for &str {
        fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
            re.is_match(&self[from..to])
        }

        fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
            let caps = re.captures(&self[from..to])?;

            Some(RawCaptures::new(caps.iter().map(|cap| {
                cap.map(|cap| (cap.start() + from, cap.end() + from))
            })))
        }
    }
}
