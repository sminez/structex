use crate::{
    compile::Inst,
    re::{Haystack, RawCaptures, RegexEngine},
    se::{Dot, Inner, MatchesInner, TaggedCaptures},
};
use std::{mem, sync::Arc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Extract {
    pub re: usize,
    pub on_extract: Option<Box<Inst>>,
    pub on_filter: Option<Box<Inst>>,
}

impl Extract {
    /// Try to merge this Extract into an existing branch if possible.
    ///
    /// Returns None if merging was successful or Some(self) if not.
    pub(crate) fn try_merge(self, branches: &mut [Inst]) -> Option<Self> {
        for branch in branches.iter_mut() {
            match branch {
                Inst::Extract(e) if self.re == e.re => match (
                    e.on_extract.is_none(),
                    e.on_filter.is_none(),
                    self.on_extract.is_none(),
                    self.on_filter.is_none(),
                ) {
                    (true, false, false, true) => {
                        e.on_extract = self.on_extract;
                        return None;
                    }
                    (false, true, true, false) => {
                        e.on_filter = self.on_filter;
                        return None;
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        Some(self)
    }
}

pub(super) struct Iter<'s, 'h, R, H>
where
    R: RegexEngine,
    H: Haystack<R> + ?Sized,
{
    haystack: &'h H,
    ext: &'s Extract,
    inner: Arc<Inner<R>>,
    /// State-machine for driving behaviour between child branches
    state: State,
    /// The original parent dot we are extracting from
    parent: Dot,
    /// The child branch we are currently iterating over
    child: Option<Box<MatchesInner<'s, 'h, R, H>>>,
}

impl<'s, 'h, R, H> Iter<'s, 'h, R, H>
where
    R: RegexEngine,
    H: Haystack<R> + ?Sized,
{
    pub fn new(haystack: &'h H, parent: Dot, ext: &'s Extract, inner: Arc<Inner<R>>) -> Self {
        Self {
            haystack,
            ext,
            inner,
            state: State::Search { prev_end: None },
            parent,
            child: None,
        }
    }

    /// locate the next match at or after `prev_end`, skipping an empty match at `prev_end` (same
    /// semantics as the regex crate).
    fn next_captures(&self, prev_end: Option<usize>) -> Option<RawCaptures> {
        let re = &self.inner.re[self.ext.re];
        let to = self.parent.to();
        let mut pos = prev_end.unwrap_or(self.parent.from());

        loop {
            let caps = self.haystack.captures_between(re, pos, to)?;
            if caps.from() == caps.to() && Some(caps.to()) == prev_end {
                if pos >= to {
                    return None;
                }
                pos += self.haystack.char_at(pos)?.len_utf8();
                continue;
            }

            return Some(caps);
        }
    }

    fn set_extract(&mut self, dot: Dot) {
        self.child = self.ext.on_extract.as_ref().and_then(|extract| {
            MatchesInner::new(extract, self.inner.clone(), self.haystack, dot).map(Box::new)
        });
    }

    fn set_filter(&mut self, dot: Dot) {
        self.child = self.ext.on_filter.as_ref().and_then(|filter| {
            MatchesInner::new(filter, self.inner.clone(), self.haystack, dot).map(Box::new)
        });
    }
}

impl<'s, 'h, R, H> Iterator for Iter<'s, 'h, R, H>
where
    R: RegexEngine,
    H: Haystack<R> + ?Sized,
{
    type Item = TaggedCaptures<'h, H>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // If our current child iterator has something to yield then return that
            if let Some(m) = self.child.as_mut().and_then(|child| child.next()) {
                return Some(m);
            }

            // Otherwise we switch behaviour based on our current state
            self.state = match mem::replace(&mut self.state, State::Done) {
                // We have finished processing the previous match from self.ext (or we're at the
                // start of the haystack) so locate the next match and set self.child.
                State::Search { prev_end } => {
                    let from = prev_end.unwrap_or(self.parent.from());

                    match self.next_captures(prev_end) {
                        // If we have a filter branch and the match is beyond our current "from"
                        // position, set self.child to run the filter for that interval and store
                        // the match for when that new child iterator completes.
                        Some(caps) if self.ext.on_filter.is_some() && caps.from() > from => {
                            self.set_filter(Dot::range(from, caps.from()));

                            State::Held(caps)
                        }

                        // If we don't have a filter branch or the match is not beyond our current
                        // "from" position, set self.child to run extraction for the match interval.
                        Some(caps) => {
                            let prev_end = Some(caps.to());
                            self.set_extract(Dot::Captures(caps));

                            State::Search { prev_end }
                        }

                        // If we failed to find a match but we have remaining input, attempt to set
                        // self.child to run a filter for what's left
                        None => {
                            if from < self.parent.to() {
                                self.set_filter(Dot::range(from, self.parent.to()));
                            }

                            State::Done
                        }
                    }
                }

                // We have a held match from self.ext to process following a possible filter
                // branch, so set self.child to run extraction for that interval
                State::Held(caps) => {
                    let prev_end = Some(caps.to());
                    self.set_extract(Dot::Captures(caps));

                    State::Search { prev_end }
                }

                // All matches exhausted so we're done
                State::Done => return None,
            };
        }
    }
}

enum State {
    Search { prev_end: Option<usize> },
    Held(RawCaptures),
    Done,
}

#[cfg(test)]
mod tests {
    use crate::Structex;
    use regex_automata::meta::Regex;
    use simple_test_case::test_case;

    // Expected positions follow the semantics of the regex crate's find_iter / split: an empty
    // match is never yielded at the end position of the previous match.
    #[test_case("x/$/ A", "a\nb\nc", &[('A', 1, 1), ('A', 3, 3), ('A', 5, 5)]; "end of line")]
    #[test_case("x/^/ A", "a\nb\nc\n", &[('A', 0, 0), ('A', 2, 2), ('A', 4, 4), ('A', 6, 6)]; "start of line")]
    #[test_case("x/a*/ A", "baab", &[('A', 0, 0), ('A', 1, 3), ('A', 4, 4)]; "empty after non-empty")]
    #[test_case("x/^/ A", "", &[('A', 0, 0)]; "empty haystack")]
    #[test_case(
        "{ x/$/ A; y/$/ B; }",
        "a\nb\nc",
        &[('B', 0, 1), ('A', 1, 1), ('B', 1, 3), ('A', 3, 3), ('B', 3, 5), ('A', 5, 5)];
        "extract and between"
    )]
    #[test_case("y/a*/ B", "baab", &[('B', 0, 1), ('B', 3, 4)]; "between with empty matches")]
    #[test]
    fn empty_match_edge_cases(se: &str, haystack: &str, expected: &[(char, usize, usize)]) {
        let se: Structex<Regex> = Structex::new(se).unwrap();
        let matches: Vec<(char, usize, usize)> = se
            .iter_tagged_captures(haystack)
            .map(|m| (m.tag().unwrap(), m.from(), m.to()))
            .collect();

        assert_eq!(matches, expected);
    }
}
