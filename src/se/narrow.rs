use crate::{
    compile::Inst,
    re::Re,
    se::{Dot, Inner, MatchesInner},
};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Narrow {
    pub re: usize,
    pub node: Box<Inst>,
}

impl Narrow {
    pub(super) fn apply<'s, 'h, R>(
        &'s self,
        haystack: R::Haystack<'h>,
        dot: Dot,
        inner: Arc<Inner<R>>,
    ) -> Option<MatchesInner<'s, 'h, R>>
    where
        R: Re,
    {
        let (from, to) = dot.loc();
        let cap = inner.re[self.re].captures_between(haystack, from, to)?;

        MatchesInner::new(&self.node, inner, haystack, Dot::Captures(cap))
    }
}
