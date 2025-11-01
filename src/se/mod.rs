//! A structural regular expression engine that can be run with an underlying, user provided regex
//! engine.
use crate::{
    Error,
    compile::{self, Compiler, Inst},
    re::{Captures, Re},
};
use std::{fmt, ops::Deref, sync::Arc};

mod extract;
mod guard;
mod narrow;
mod parallel;

pub(crate) use extract::Extract;
pub(crate) use guard::Guard;
pub(crate) use narrow::Narrow;

#[derive(Clone)]
pub struct Structex<R>
where
    R: Re,
{
    raw: Arc<str>,
    inner: Arc<Inner<R>>,
}

impl<R> fmt::Debug for Structex<R>
where
    R: Re,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Structex").field(&self.raw).finish()
    }
}

impl<R> fmt::Display for Structex<R>
where
    R: Re,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Structex({})", self.raw)
    }
}

impl<R> Structex<R>
where
    R: Re,
{
    pub fn compile(se: &str) -> Result<Self, Error> {
        StructexBuilder::default().build(se)
    }

    pub fn as_str(&self) -> &str {
        &self.raw
    }

    pub fn action_content(&self) -> &[String] {
        &self.inner.action_content
    }

    pub fn tags(&self) -> &str {
        &self.inner.tags
    }

    pub fn iter_matches<'h>(&'h self, haystack: &'h str) -> MatchIter<'h, R> {
        MatchIter::new(&self.inner.inst, self.inner.clone(), haystack)
    }
}

trait ActionContentFn: Fn(String) -> String + 'static {}
impl<F> ActionContentFn for F where F: Fn(String) -> String + 'static {}

fn raw_content_string(s: String) -> String {
    s
}

fn escaped_content_string(s: String) -> String {
    s.replace("\\n", "\n").replace("\\t", "\t")
}

pub struct StructexBuilder {
    action_content_fn: Box<dyn ActionContentFn>,
    allowed_argless_tags: Option<String>,
    allowed_single_arg_tags: Option<String>,
}

impl StructexBuilder {
    pub fn with_raw_content_string(mut self) -> Self {
        self.action_content_fn = Box::new(raw_content_string);
        self
    }

    pub fn with_escaped_content_string(mut self) -> Self {
        self.action_content_fn = Box::new(escaped_content_string);
        self
    }

    pub fn with_content_string_fn<F>(mut self, f: F) -> Self
    where
        F: Fn(String) -> String + 'static,
    {
        self.action_content_fn = Box::new(f);
        self
    }

    pub fn with_allowed_argless_tags(mut self, tags: impl Into<String>) -> Self {
        self.allowed_argless_tags = Some(tags.into());
        self
    }

    pub fn with_allowed_single_arg_tags(mut self, tags: impl Into<String>) -> Self {
        self.allowed_single_arg_tags = Some(tags.into());
        self
    }

    pub fn build<R>(self, se: &str) -> Result<Structex<R>, Error>
    where
        R: Re,
    {
        let mut c = Compiler {
            allowed_argless_tags: self.allowed_argless_tags,
            allowed_single_arg_tags: self.allowed_single_arg_tags,
            ..Default::default()
        };

        let inst = c.compile(se)?;
        let Compiler {
            re,
            tags,
            action_content,
            ..
        } = c;

        Ok(Structex {
            raw: Arc::from(se),
            inner: Arc::new(Inner {
                inst,
                re: re
                    .into_iter()
                    .map(|re| R::compile(&re).map_err(|e| Error::InvalidRegex(Box::new(e))))
                    .collect::<Result<Vec<_>, _>>()?,
                tags,
                action_content: action_content
                    .into_iter()
                    .map(self.action_content_fn)
                    .collect(),
            }),
        })
    }
}

impl Default for StructexBuilder {
    fn default() -> Self {
        Self {
            action_content_fn: Box::new(escaped_content_string),
            allowed_argless_tags: None,
            allowed_single_arg_tags: None,
        }
    }
}

pub(super) struct Inner<R>
where
    R: Re,
{
    pub(super) inst: Inst,
    pub(super) re: Vec<R>,
    pub(super) tags: String,
    pub(super) action_content: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Dot {
    Range { from: usize, to: usize },
    Captures(Captures),
}

impl Dot {
    pub fn loc(&self) -> (usize, usize) {
        match self {
            Self::Range { from, to } => (*from, *to),
            Self::Captures(caps) => caps.get_match(),
        }
    }

    pub fn from(&self) -> usize {
        match self {
            Self::Range { from, .. } => *from,
            Self::Captures(caps) => caps.from(),
        }
    }

    pub fn to(&self) -> usize {
        match self {
            Self::Range { to, .. } => *to,
            Self::Captures(caps) => caps.to(),
        }
    }

    fn into_captures(self) -> Captures {
        match self {
            Self::Range { from, to } => Captures::new(std::iter::once(Some((from, to)))),
            Self::Captures(c) => c,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub captures: Captures,
    pub action: Option<Action>,
}

impl Deref for Match {
    type Target = Captures;

    fn deref(&self) -> &Self::Target {
        &self.captures
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Action {
    pub tag: char,
    pub content: Option<String>,
}

impl Action {
    fn new<R>(a: &compile::Action, inner: &Inner<R>) -> Self
    where
        R: Re,
    {
        Self {
            tag: a.tag,
            content: a.content.map(|idx| inner.action_content[idx].clone()),
        }
    }
}

pub struct MatchIter<'h, R>
where
    R: Re,
{
    inner: Option<MatchIterInner<'h, R>>,
}

impl<'h, R> MatchIter<'h, R>
where
    R: Re,
{
    fn new(inst: &'h Inst, inner: Arc<Inner<R>>, haystack: &'h str) -> Self {
        Self {
            inner: MatchIterInner::new(
                inst,
                inner,
                haystack,
                Dot::Range {
                    from: 0,
                    to: haystack.len(),
                },
            ),
        }
    }
}

impl<'h, R> Iterator for MatchIter<'h, R>
where
    R: Re,
{
    type Item = Match;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.as_mut().and_then(|inner| inner.next())
    }
}

enum MatchIterInner<'h, R>
where
    R: Re,
{
    Extract(extract::Iter<'h, R>),
    Parallel(parallel::Iter<'h, R>),
    Emit(Option<Match>),
}

impl<'h, R> MatchIterInner<'h, R>
where
    R: Re,
{
    fn new(inst: &'h Inst, inner: Arc<Inner<R>>, haystack: &'h str, dot: Dot) -> Option<Self> {
        match inst {
            // EmitMatch and Action just emit their value
            Inst::EmitMatch => Some(Self::Emit(Some(Match {
                captures: dot.into_captures(),
                action: None,
            }))),
            Inst::Action(a) => Some(Self::Emit(Some(Match {
                captures: dot.into_captures(),
                action: Some(Action::new(a, &inner)),
            }))),

            // Narrow and Guard act as filters on the instructions they wrap
            Inst::Narrow(n) => n.apply(haystack, dot, inner),
            Inst::Guard(g) => g.apply(haystack, dot, inner),

            // Extract and Parallel are actual iterators
            Inst::Extract(ext) => {
                Some(Self::Extract(extract::Iter::new(haystack, dot, ext, inner)))
            }
            Inst::Parallel(bs) => Some(Self::Parallel(parallel::Iter::new(
                haystack, dot, bs, inner,
            ))),
        }
    }
}

impl<'h, R> Iterator for MatchIterInner<'h, R>
where
    R: Re,
{
    type Item = Match;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Extract(ext) => ext.next(),
            Self::Parallel(p) => p.next(),
            Self::Emit(opt) => opt.take(),
        }
    }
}
