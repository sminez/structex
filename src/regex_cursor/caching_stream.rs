use crate::{
    TaggedCaptures,
    re::{Sliceable, Writable},
};
use regex_cursor::{Cursor, Input};
use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt,
    io::{self, BufRead, BufReader, Read},
    mem,
    ops::Range,
};

/// The maximum number of line allocations kept in cache for reuse.
const MAX_CACHED_LINES: usize = 32;

/// A haystack that reads UTF-8 text from a [Read] one line at a time as it is searched, caching
/// what has been read.
///
/// Searching a `CachingStream` only reads as much input as is needed to locate each match, so it
/// can be used with unbounded input such as stdin. To avoid buffering the entire input, call
/// [drop_before][CachingStream::drop_before] once you are done with each [TaggedCaptures].
///
/// # Errors
/// Read errors (including invalid UTF-8) are treated as the end of the input. Use
/// [has_error][CachingStream::has_error] to check whether or not end of iteration was due to an
/// error and [take_error][CachingStream::take_error] to extract the underlying [io::Error] itself.
#[derive(Debug)]
pub struct CachingStream<R>
where
    R: Read,
{
    inner: RefCell<Inner<R>>,
}

impl<R> CachingStream<R>
where
    R: Read,
{
    /// Create a new stream wrapping the given [reader][Read].
    pub fn new(reader: R) -> Self {
        Self {
            inner: RefCell::new(Inner {
                reader: BufReader::new(reader),
                lines: VecDeque::new(),
                line_cache: Vec::with_capacity(MAX_CACHED_LINES),
                bytes_read: 0,
                state: State::Reading,
            }),
        }
    }

    /// Drop all buffered input that was read before `caps`.
    ///
    /// Everything before the line containing the character preceding `caps` is dropped.
    pub fn drop_before(&self, caps: &TaggedCaptures<'_, Self>) {
        self.inner.borrow_mut().drop_before(caps.from());
    }

    /// Whether or not an error was encountered while reading from the underlying [reader][Read].
    pub fn has_error(&self) -> bool {
        matches!(self.inner.borrow_mut().state, State::Error(_))
    }

    /// The error that ended reading from the underlying [reader][Read], if there was one.
    pub fn take_error(&self) -> Option<io::Error> {
        let mut inner = self.inner.borrow_mut();

        match mem::take(&mut inner.state) {
            State::Error(e) => Some(e),
            State::Reading => None,
            State::AtEof => {
                inner.state = State::AtEof;
                None
            }
        }
    }

    /// Unwrap an [io::Error] from the underlying [reader][Read].
    ///
    /// # Panics
    /// Panics if there is no error to return.
    pub fn unwrap_error(&self) -> io::Error {
        let mut inner = self.inner.borrow_mut();

        match mem::take(&mut inner.state) {
            State::Error(e) => e,
            _ => panic!("unwrap_error called when no read error was present"),
        }
    }

    pub(super) fn with_input<T>(
        &self,
        from: usize,
        to: usize,
        end_anchored: bool,
        f: impl FnOnce(Input<&mut StreamCursor<'_, R>>) -> T,
    ) -> T {
        let mut inner = self.inner.borrow_mut();

        if end_anchored {
            inner.read_to_eof();
        } else {
            inner.read_past(from);
        }

        let mut cursor = inner.cursor(from);
        let mut input = Input::new(&mut cursor);

        if to == usize::MAX {
            input.set_start(from);
        } else {
            input.set_span(from..to);
        }

        f(input)
    }
}

impl<R> Sliceable for CachingStream<R>
where
    R: Read,
{
    type Slice<'h>
        = StreamSlice<'h, R>
    where
        Self: 'h;

    fn char_at(&self, byte_offset: usize) -> Option<char> {
        let mut inner = self.inner.borrow_mut();
        inner.read_past(byte_offset);

        if byte_offset >= inner.bytes_read {
            return None;
        }

        let line = &inner.lines[inner.line_index_of(byte_offset)];

        line.text[byte_offset - line.start..].chars().next()
    }

    fn slice(&self, range: Range<usize>) -> Self::Slice<'_> {
        StreamSlice {
            inner: &self.inner,
            range,
        }
    }

    fn max_len(&self) -> usize {
        let mut inner = self.inner.borrow_mut();
        inner.read_to_eof();

        inner.bytes_read
    }

    fn known_len(&self) -> Option<usize> {
        let inner = self.inner.borrow();

        if inner.state.is_reading() {
            None
        } else {
            Some(inner.bytes_read)
        }
    }
}

impl<R> Writable for CachingStream<R>
where
    R: Read,
{
    fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
    where
        W: io::Write,
    {
        let inner = self.inner.borrow();
        let mut n = 0;

        for line in inner.lines.iter() {
            w.write_all(line.text.as_bytes())?;
            n += line.text.len();
        }

        Ok(n)
    }
}

/// A slice from a [CachingStream].
#[derive(Debug)]
pub struct StreamSlice<'a, R>
where
    R: Read,
{
    inner: &'a RefCell<Inner<R>>,
    range: Range<usize>,
}

impl<R> StreamSlice<'_, R>
where
    R: Read,
{
    fn try_for_each<E>(&self, mut f: impl FnMut(&str) -> Result<(), E>) -> Result<(), E> {
        let inner = self.inner.borrow();
        let Range { start, end } = self.range;

        for line in inner.lines.range(inner.line_index_of(start)..) {
            let from = start.max(line.start) - line.start;
            let to = end.min(line.end()) - line.start;

            if from < to {
                f(&line.text[from..to])?;
            }

            if line.end() >= end {
                break;
            }
        }

        Ok(())
    }
}

impl<R> Writable for StreamSlice<'_, R>
where
    R: Read,
{
    fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
    where
        W: io::Write,
    {
        self.try_for_each(|s| w.write_all(s.as_bytes()))?;

        Ok(self.range.len())
    }
}

impl<R> fmt::Display for StreamSlice<'_, R>
where
    R: Read,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.try_for_each(|s| f.write_str(s))
    }
}

#[derive(Default, Debug)]
struct Line {
    start: usize,
    text: String,
}

impl Line {
    fn end(&self) -> usize {
        self.start + self.text.len()
    }
}

#[derive(Default, Debug)]
enum State {
    #[default]
    Reading,
    AtEof,
    Error(io::Error),
}

impl State {
    const fn is_reading(&self) -> bool {
        matches!(self, Self::Reading)
    }
}

#[derive(Debug)]
struct Inner<R>
where
    R: Read,
{
    reader: BufReader<R>,
    state: State,
    lines: VecDeque<Line>,
    line_cache: Vec<Line>,
    bytes_read: usize,
}

impl<R> Inner<R>
where
    R: Read,
{
    fn line_index_of(&self, offset: usize) -> usize {
        self.lines
            .partition_point(|l| l.end() <= offset)
            .min(self.lines.len().saturating_sub(1))
    }

    fn drop_before(&mut self, offset: usize) {
        while let Some(line) = self.lines.front()
            && line.end() < offset
        {
            let line = self.lines.pop_front().unwrap();
            if self.line_cache.len() < MAX_CACHED_LINES {
                self.line_cache.push(line);
            }
        }
    }

    fn cursor(&mut self, at: usize) -> StreamCursor<'_, R> {
        let idx = self.line_index_of(at);

        StreamCursor {
            inner: self,
            line_idx: idx,
        }
    }

    fn try_read_line(&mut self) {
        if !self.state.is_reading() {
            return;
        }

        let mut line = self.line_cache.pop().unwrap_or_default();
        line.text.clear();

        match self.reader.read_line(&mut line.text) {
            Err(e) => self.state = State::Error(e),
            Ok(0) => self.state = State::AtEof,
            Ok(n) => {
                line.start = self.bytes_read;
                self.lines.push_back(line);
                self.bytes_read += n;
            }
        }
    }

    fn read_past(&mut self, offset: usize) {
        while self.bytes_read <= offset && self.state.is_reading() {
            self.try_read_line();
        }
    }

    fn read_to_eof(&mut self) {
        while self.state.is_reading() {
            self.try_read_line();
        }
    }
}

/// A [Cursor] over the lines of a [CachingStream], reading further lines as it advances.
pub(super) struct StreamCursor<'a, R>
where
    R: Read,
{
    inner: &'a mut Inner<R>,
    line_idx: usize,
}

impl<R> Cursor for StreamCursor<'_, R>
where
    R: Read,
{
    fn chunk(&self) -> &[u8] {
        self.inner
            .lines
            .get(self.line_idx)
            .map_or(&[], |l| l.text.as_bytes())
    }

    fn advance(&mut self) -> bool {
        let can_advance = if self.line_idx + 1 < self.inner.lines.len() {
            true
        } else {
            self.inner.try_read_line();
            self.inner.state.is_reading()
        };

        if can_advance {
            self.line_idx += 1;
        }

        can_advance
    }

    fn backtrack(&mut self) -> bool {
        if self.line_idx == 0 {
            false
        } else {
            self.line_idx -= 1;
            true
        }
    }

    fn total_bytes(&self) -> Option<usize> {
        match self.inner.state {
            State::Reading => None,
            _ => Some(self.inner.bytes_read),
        }
    }

    fn offset(&self) -> usize {
        self.inner
            .lines
            .get(self.line_idx)
            .map_or(self.inner.bytes_read, |l| l.start)
    }
}
