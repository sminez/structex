use crate::{
    compile::Inst,
    re::{Haystack, Re},
    se::{Dot, Inner, MatchesInner},
};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Narrow {
    pub re: usize,
    pub node: Box<Inst>,
}

impl Narrow {
    pub(super) fn apply<'s, R, H>(
        &'s self,
        haystack: H,
        dot: Dot,
        inner: Arc<Inner<R>>,
    ) -> Option<MatchesInner<'s, R, H>>
    where
        R: Re,
        H: Haystack<R>,
    {
        let (from, to) = dot.loc();
        let cap = haystack.captures_between(&inner.re[self.re], from, to)?;

        MatchesInner::new(&self.node, inner, haystack, Dot::Captures(cap))
    }
}
