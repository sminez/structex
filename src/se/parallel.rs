use crate::{
    compile::Inst,
    re::Re,
    se::{Dot, Inner, MatchesInner, TaggedCaptures},
};
use std::sync::Arc;

pub(super) struct Iter<'s, 'h, R>
where
    R: Re,
{
    branches: Vec<Branch<'s, 'h, R>>,
}

impl<'s, 'h, R> Iter<'s, 'h, R>
where
    R: Re,
{
    pub fn new(
        haystack: R::Haystack<'h>,
        dot: Dot,
        branches: &'s [Inst],
        inner: Arc<Inner<R>>,
    ) -> Self {
        let branches = branches
            .iter()
            .flat_map(|inst| {
                MatchesInner::new(inst, inner.clone(), haystack, dot.clone())
                    .map(|it| Branch { held: None, it })
            })
            .collect();

        Self { branches }
    }
}

impl<'s, 'h, R> Iterator for Iter<'s, 'h, R>
where
    R: Re,
{
    type Item = TaggedCaptures<R::Haystack<'h>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.branches.retain_mut(|b| b.update());

        match self.branches.len() {
            0 => None,
            1 => self.branches[0].held.take(),
            _ => {
                let mut next = self.branches[0].get_match().unwrap();
                let mut ix = 0;

                // Matches coming from a parallel group are yielded in order of their position
                // within the haystack. Shorter matches starting from the same offset are yielded
                // first and in the case of equal offsets we order based on branch ordering.
                for (i, b) in self.branches[1..].iter().enumerate() {
                    let m = b.get_match().unwrap();
                    if m < next {
                        next = m;
                        ix = i + 1; // enumerate is one off
                    }
                }

                self.branches[ix].held.take()
            }
        }
    }
}

struct Branch<'s, 'h, R>
where
    R: Re,
{
    held: Option<TaggedCaptures<R::Haystack<'h>>>,
    it: MatchesInner<'s, 'h, R>,
}

impl<'s, 'h, R> Branch<'s, 'h, R>
where
    R: Re,
{
    fn get_match(&self) -> Option<(usize, usize)> {
        self.held.as_ref().map(|m| m.get_match())
    }

    /// Returns false when there is no held match and the inner iterator is complete
    fn update(&mut self) -> bool {
        if self.held.is_none() {
            self.held = self.it.next();
        }

        self.held.is_some()
    }
}
