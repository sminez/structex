use crate::{
    Re,
    compile::Inst,
    se::{Dot, Inner, MatchesInner},
};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Narrow {
    pub re: usize,
    pub node: Box<Inst>,
}

impl Narrow {
    pub(super) fn apply<'h, R>(
        &'h self,
        haystack: &'h str,
        dot: Dot,
        inner: Arc<Inner<R>>,
    ) -> Option<MatchesInner<'h, R>>
    where
        R: Re,
    {
        let (from, to) = dot.loc();
        let cap = inner.re[self.re].captures_between(haystack, from, to)?;

        MatchesInner::new(&self.node, inner, haystack, Dot::Captures(cap))
    }
}
