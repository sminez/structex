use crate::{
    compile::Inst,
    re::{Haystack, RegexEngine},
    se::{Dot, Inner, MatchesInner},
};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Narrow {
    pub re: usize,
    pub node: Box<Inst>,
}

impl Narrow {
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
        let cap = haystack.captures_between(&inner.re[self.re], from, to)?;

        MatchesInner::new(&self.node, inner, haystack, Dot::Captures(cap))
    }
}
