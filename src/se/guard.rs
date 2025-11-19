use crate::{
    compile::Inst,
    re::{Haystack, RegexEngine},
    se::{Dot, Inner, MatchesInner},
};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Guard {
    pub re: usize,
    pub if_matching: Option<Box<Inst>>,
    pub if_not_matching: Option<Box<Inst>>,
}

impl Guard {
    /// Try to merge this Guard into an existing branch if possible.
    ///
    /// Returns None if merging was successful or Some(self) if not.
    pub(crate) fn try_merge(self, branches: &mut [Inst]) -> Option<Self> {
        for branch in branches.iter_mut() {
            match branch {
                Inst::Guard(g) if self.re == g.re => match (
                    g.if_matching.is_none(),
                    g.if_not_matching.is_none(),
                    self.if_matching.is_none(),
                    self.if_not_matching.is_none(),
                ) {
                    (true, false, false, true) => {
                        g.if_matching = self.if_matching;
                        return None;
                    }
                    (false, true, true, false) => {
                        g.if_not_matching = self.if_not_matching;
                        return None;
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        Some(self)
    }

    pub(super) fn apply<'s, 'h, R, H>(
        &'s self,
        haystack: &'h H,
        dot: Dot,
        inner: Arc<Inner<R>>,
    ) -> Option<MatchesInner<'s, 'h, R, H>>
    where
        R: RegexEngine,
        H: Haystack<R> + ?Sized,
    {
        let (from, to) = dot.loc();
        let is_match = haystack.is_match_between(&inner.re[self.re], from, to);
        match (
            is_match,
            self.if_matching.as_ref(),
            self.if_not_matching.as_ref(),
        ) {
            (true, Some(inst), _) => MatchesInner::new(inst, inner, haystack, dot),
            (false, _, Some(inst)) => MatchesInner::new(inst, inner, haystack, dot),
            _ => None,
        }
    }
}
