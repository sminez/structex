use crate::{
    Re,
    compile::Inst,
    se::{Dot, Inner},
};
use std::sync::Arc;

pub(super) struct Iter<'h, R>
where
    R: Re,
{
    haystack: &'h str,
    dot: Dot,
    branches: &'h [Inst],
    inner: Arc<Inner<R>>,
}

impl<'h, R> Iter<'h, R>
where
    R: Re,
{
    pub fn new(haystack: &'h str, dot: Dot, branches: &'h [Inst], inner: Arc<Inner<R>>) -> Self {
        Self {
            haystack,
            dot,
            branches,
            inner,
        }
    }
}
