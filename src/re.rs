//! The required interface for an underlying regex engine
pub trait Re: Sized {
    type Error: std::error::Error + 'static;

    fn compile(re: &str) -> Result<Self, Self::Error>;
    fn matches(&self, haystack: &str) -> bool;
    fn captures(&self, haystack: &str) -> Option<Captures>;

    fn matches_between(&self, haystack: &str, from: usize, to: usize) -> bool {
        self.matches(&haystack[from..to])
    }

    fn captures_between(&self, haystack: &str, from: usize, to: usize) -> Option<Captures> {
        let mut caps = self.captures(&haystack[from..to])?;
        caps.apply_offset(from);

        Some(caps)
    }
}

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

impl Re for regex::Regex {
    type Error = regex::Error;

    fn compile(re: &str) -> Result<Self, Self::Error> {
        regex::RegexBuilder::new(re).multi_line(true).build()
    }

    fn matches(&self, haystack: &str) -> bool {
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
