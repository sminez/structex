use crate::{
    compile::Inst,
    re::{RawCaptures, Re},
    se::{Dot, Inner, MatchesInner, TaggedCaptures},
};
use std::sync::Arc;

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

pub(super) struct Iter<'s, 'h, R>
where
    R: Re,
{
    haystack: R::Haystack<'h>,
    ext: &'s Extract,
    inner: Arc<Inner<R>>,
    /// The original parent dot we are extracting from
    parent: Dot,
    /// The child branch we are currently iterating over
    child: Option<Box<MatchesInner<'s, 'h, R>>>,
    /// The current match
    held: Option<RawCaptures>,
    /// The current byte offset we are up to
    pos: usize,
}

impl<'s, 'h, R> Iter<'s, 'h, R>
where
    R: Re,
{
    pub fn new(
        haystack: R::Haystack<'h>,
        parent: Dot,
        ext: &'s Extract,
        inner: Arc<Inner<R>>,
    ) -> Self {
        let pos = parent.from();

        Self {
            haystack,
            ext,
            inner,
            parent,
            child: None,
            held: None,
            pos,
        }
    }

    fn next_captures(&self) -> Option<RawCaptures> {
        self.inner.re[self.ext.re].captures_between(self.haystack, self.pos, self.parent.to())
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

impl<'s, 'h, R> Iterator for Iter<'s, 'h, R>
where
    R: Re,
{
    type Item = TaggedCaptures<R::Haystack<'h>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // If our current child iterator has something to yield then return that
            let next = self.child.as_mut().and_then(|child| child.next());
            if next.is_some() {
                return next;
            }

            // If we've reached the end of the input we were given to search then we're done
            if self.pos == self.parent.to() {
                return None;
            }

            // If we have a held match then update child
            if let Some(caps) = self.held.take() {
                self.pos = caps.to();
                self.set_extract(Dot::Captures(caps));
            } else {
                // Otherwise find the next match
                match self.next_captures() {
                    Some(caps) => {
                        if self.ext.on_filter.is_some() && caps.from() > self.pos {
                            self.set_filter(Dot::Range {
                                from: self.pos,
                                to: caps.from(),
                            });
                            self.pos = caps.from();
                            self.held = Some(caps);
                        } else {
                            self.pos = caps.to();
                            self.set_extract(Dot::Captures(caps));
                        }
                    }

                    None => {
                        self.set_filter(Dot::Range {
                            from: self.pos,
                            to: self.parent.to(),
                        });
                        self.pos = self.parent.to();
                    }
                }
            }
        }
    }
}
