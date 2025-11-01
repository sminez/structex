use crate::{
    Re,
    compile::Inst,
    se::{Dot, Inner, Match, MatchIterInner},
};
use std::sync::Arc;

pub(super) struct Iter<'h, R>
where
    R: Re,
{
    branches: Vec<Branch<'h, R>>,
}

impl<'h, R> Iter<'h, R>
where
    R: Re,
{
    pub fn new(haystack: &'h str, dot: Dot, branches: &'h [Inst], inner: Arc<Inner<R>>) -> Self {
        let branches = branches
            .iter()
            .flat_map(|inst| {
                MatchIterInner::new(inst, inner.clone(), haystack, dot.clone())
                    .map(|it| Branch { held: None, it })
            })
            .collect();

        Self { branches }
    }
}

impl<'h, R> Iterator for Iter<'h, R>
where
    R: Re,
{
    type Item = Match;

    fn next(&mut self) -> Option<Self::Item> {
        self.branches.retain_mut(|b| b.update());

        match self.branches.len() {
            0 => None,
            1 => self.branches[0].held.take(),
            _ => {
                let mut next = self.branches[0].get_match().unwrap();
                let mut ix = 0;

                for (i, b) in self.branches[1..].iter().enumerate() {
                    let m = b.get_match().unwrap();
                    if m < next {
                        next = m;
                        ix = i;
                    }
                }

                self.branches[ix].held.take()
            }
        }
    }
}

struct Branch<'h, R>
where
    R: Re,
{
    held: Option<Match>,
    it: MatchIterInner<'h, R>,
}

impl<'h, R> Branch<'h, R>
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
