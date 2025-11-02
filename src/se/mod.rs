//! A structural regular expression engine that can be run with an underlying, user provided regex
//! engine.
use crate::{
    Error,
    compile::{Compiler, Inst},
    re::{Captures, RawCaptures, Re},
};
use std::{fmt, ops::Deref, sync::Arc};

mod extract;
mod guard;
mod narrow;
mod parallel;

pub(crate) use extract::Extract;
pub(crate) use guard::Guard;
pub(crate) use narrow::Narrow;

/// A compiled structural regular expression backed by an underlying regular expression engine.
///
/// A `Structex` can be used to search for tagged substrings within a haystack supported by the
/// regular expression engine it is backed by. The primary API for making use of a `Structex` is
/// the [iter_matches][Structex::iter_matches] method which will iterate over the tagged
/// [Matches][Match] within a given haystack as it is searched.
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
    /// Compiles a structural regular expression. Once compiled it may be used repeatedly and cloned
    /// cheaply, but note that compilation can be an expensive process so [Structex] instances
    /// should be reused wherever possible.
    ///
    /// To configure how given `Structex` is compiled, see [StructexBuilder].
    ///
    /// # Error
    /// If an invalid expression is given then an error is returned. The exact expressions that are
    /// valid to compile will depend on the underlying regular expression engine being used.
    ///
    /// # Example
    /// ```
    /// // A Structex backed by the regex crate
    /// type Structex = structex::Structex<regex::Regex>;
    ///
    /// // An empty expression is always invalid
    /// assert!(Structex::new("").is_err());
    ///
    /// // The top level expression must not be a bare action
    /// assert!(Structex::new("P/I am invalid/").is_err());
    ///
    /// // A valid
    /// assert!(Structex::new("x/hello, (world|sailor)!/ p").is_ok());
    /// ```
    pub fn new(se: &str) -> Result<Self, Error> {
        StructexBuilder::default().build(se)
    }

    /// Returns the original string of this structex.
    ///
    /// # Example
    /// ```
    /// type Structex = structex::Structex<regex::Regex>;
    ///
    /// let se = Structex::new("x/foo.*bar/ p").unwrap();
    /// assert_eq!(se.as_str(), "x/foo.*bar/ p");
    /// ```
    pub fn as_str(&self) -> &str {
        &self.raw
    }

    /// Returns the registered [actions][Action] that were parsed from the compiled expression.
    ///
    /// # Example
    /// ```
    /// use structex::Action;
    ///
    /// type Structex = structex::Structex<regex::Regex>;
    ///
    /// let se = Structex::new("x/foo.*bar/ { p; a/baz/; }").unwrap();
    /// let actions = se.actions();
    ///
    /// assert_eq!(actions.len(), 2);
    /// assert_eq!(actions[0].as_ref(), &Action { tag: 'p', arg: None });
    /// assert_eq!(actions[1].as_ref(), &Action { tag: 'a', arg: Some("baz".to_string()) });
    /// ```
    pub fn actions(&self) -> &[Arc<Action>] {
        &self.inner.actions
    }

    /// Returns the registered tags that were parsed from the compiled expression.
    ///
    /// # Example
    /// ```
    /// type Structex = structex::Structex<regex::Regex>;
    ///
    /// let se = Structex::new("x/foo.*bar/ { p; a/baz/; }").unwrap();
    /// assert_eq!(se.tags(), &['a', 'p']);
    /// ```
    pub fn tags(&self) -> &[char] {
        &self.inner.tags
    }

    /// Iterate over all tagged [matches][Match] within the given haystack in order.
    ///
    /// # Example
    ///
    /// By default, matches will be emitted without an associated action attached to them,
    /// allowing you to write simple expressions that filter and refine regions of the haystack to
    /// locate the structure you are looking for.
    /// ```
    /// type Structex = structex::Structex<regex::Regex>;
    ///
    /// let se = Structex::new(r#"
    ///   x/(.|\n)*?\./   # split into sentences
    ///   g/Alice/        # if the sentence contains "Alice"
    ///   n/(\w+)\./      # extract the last word of the sentence
    /// "#).unwrap();
    ///
    /// let haystack = r#"This is a multi-line
    /// string that mentions peoples names.
    /// People like Alice and Bob. People
    /// like Claire and David, but really
    /// we're here to talk about Alice.
    /// Alice is everyone's friend."#;
    ///
    /// let last_words: Vec<&str> = se
    ///     .iter_matches(haystack)
    ///     .map(|m| m.submatch_text(1).unwrap())
    ///     .collect();
    ///
    /// assert_eq!(&last_words, &["Bob", "Alice", "friend"]);
    /// ```
    ///
    /// When writing more complex expressions you will want to assign tagged actions to each
    /// matching branch in order to distinguish them:
    /// ```
    /// type Structex = structex::Structex<regex::Regex>;
    ///
    /// let se = Structex::new(r#"
    ///   ## split into sentences
    ///   x/(.|\n)*?\./ {
    ///     ## if the sentence contains "Alice" extract the last word of the sentence
    ///     g/Alice/ n/(\w+)\./ A;
    ///     ## if it doesn't, extract the first word of the sentence
    ///     v/Alice/ n/(\w+)/ B;
    ///   }
    /// "#).unwrap();
    ///
    /// let haystack = r#"This is a multi-line
    /// string that mentions peoples names.
    /// People like Alice and Bob. People
    /// like Claire and David, but really
    /// we're here to talk about Alice.
    /// Alice is everyone's friend."#;
    ///
    /// let words: Vec<(char, &str)> = se
    ///     .iter_matches(haystack)
    ///     .map(|m| (m.tag().unwrap(), m.submatch_text(1).unwrap()))
    ///     .collect();
    ///
    /// assert_eq!(
    ///     &words,
    ///     &[('B', "This"), ('A', "Bob"), ('A', "Alice"), ('A', "friend")]
    /// );
    /// ```
    pub fn iter_matches<'h>(&'h self, haystack: &'h str) -> MatchIter<'h, R> {
        MatchIter::new(&self.inner.inst, self.inner.clone(), haystack)
    }
}

trait ActionArgFn: Fn(String) -> String + 'static {}
impl<F> ActionArgFn for F where F: Fn(String) -> String + 'static {}

fn raw_arg_string(s: String) -> String {
    s
}

fn escaped_arg_string(s: String) -> String {
    s.replace("\\n", "\n").replace("\\t", "\t")
}

pub struct StructexBuilder {
    action_content_fn: Box<dyn ActionArgFn>,
    allowed_argless_tags: Option<String>,
    allowed_single_arg_tags: Option<String>,
}

impl StructexBuilder {
    pub fn with_raw_arg_strings(mut self) -> Self {
        self.action_content_fn = Box::new(raw_arg_string);
        self
    }

    pub fn with_escaped_arg_strings(mut self) -> Self {
        self.action_content_fn = Box::new(escaped_arg_string);
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
            re, tags, actions, ..
        } = c;

        let actions: Vec<_> = actions
            .into_iter()
            .map(|mut a| {
                a.arg = a.arg.take().map(|s| (self.action_content_fn)(s));
                Arc::new(a)
            })
            .collect();

        Ok(Structex {
            raw: Arc::from(se),
            inner: Arc::new(Inner {
                inst,
                re: re
                    .into_iter()
                    .map(|re| R::compile(&re).map_err(|e| Error::Regex(Box::new(e))))
                    .collect::<Result<Vec<_>, _>>()?,
                tags,
                actions,
            }),
        })
    }
}

impl Default for StructexBuilder {
    fn default() -> Self {
        Self {
            action_content_fn: Box::new(escaped_arg_string),
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
    pub(super) tags: Vec<char>,
    pub(super) actions: Vec<Arc<Action>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Dot {
    Range { from: usize, to: usize },
    Captures(RawCaptures),
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

    fn into_stubbed_captures<'h>(self) -> Captures<'h> {
        match self {
            Self::Range { from, to } => Captures::stubbed(vec![Some((from, to))]),
            Self::Captures(c) => Captures::stubbed(c.caps),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match<'h> {
    pub captures: Captures<'h>,
    pub action: Option<Arc<Action>>,
}

impl<'h> Match<'h> {
    pub fn tag(&self) -> Option<char> {
        self.action.as_ref().map(|a| a.tag)
    }

    pub fn arg(&self) -> Option<&str> {
        self.action.as_ref().and_then(|a| a.arg.as_deref())
    }

    pub fn has_action(&self) -> bool {
        self.action.is_some()
    }
}

impl<'h> Deref for Match<'h> {
    type Target = Captures<'h>;

    fn deref(&self) -> &Self::Target {
        &self.captures
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Action {
    pub tag: char,
    pub arg: Option<String>,
}

pub struct MatchIter<'h, R>
where
    R: Re,
{
    haystack: &'h str,
    inner: Option<MatchIterInner<'h, R>>,
}

impl<'h, R> MatchIter<'h, R>
where
    R: Re,
{
    fn new(inst: &'h Inst, inner: Arc<Inner<R>>, haystack: &'h str) -> Self {
        Self {
            haystack,
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
    type Item = Match<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.as_mut().and_then(|inner| {
            inner.next().map(|mut m| {
                m.captures.set_haystack(self.haystack);
                m
            })
        })
    }
}

enum MatchIterInner<'h, R>
where
    R: Re,
{
    Extract(extract::Iter<'h, R>),
    Parallel(parallel::Iter<'h, R>),
    Emit(Option<Match<'h>>),
}

impl<'h, R> MatchIterInner<'h, R>
where
    R: Re,
{
    fn new(inst: &'h Inst, inner: Arc<Inner<R>>, haystack: &'h str, dot: Dot) -> Option<Self> {
        match inst {
            // EmitMatch and Action just emit their value
            Inst::EmitMatch => Some(Self::Emit(Some(Match {
                captures: dot.into_stubbed_captures(),
                action: None,
            }))),
            Inst::Action(i) => Some(Self::Emit(Some(Match {
                captures: dot.into_stubbed_captures(),
                action: Some(inner.actions[*i].clone()),
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
    type Item = Match<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Extract(ext) => ext.next(),
            Self::Parallel(p) => p.next(),
            Self::Emit(opt) => opt.take(),
        }
    }
}
