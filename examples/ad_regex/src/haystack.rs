//! A haystack is something that can be searched over by a Regex
use crate::gap_buffer::GapBuffer;
use std::borrow::Cow;

/// Something that can be searched over by a [Regex][0].
///
/// The interface exposed by this trait supports searching over streaming data if needed but at the
/// cost of reduced performance.
///
/// [0]: crate::regex::Regex
#[allow(clippy::len_without_is_empty)]
pub trait Haystack {
    fn try_make_contiguous(&mut self);
    fn is_contiguous(&self) -> bool;
    fn len(&self) -> usize;
    fn substr_from<'a>(&'a self, offset: usize) -> Option<Cow<'a, str>>;
    fn substr<'a>(&'a self, from: usize, to: usize) -> Cow<'a, str>;

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize>;
    fn char_to_byte(&self, char_idx: usize) -> Option<usize>;

    fn iter_from(&self, from: usize) -> Option<impl Iterator<Item = (usize, char)>>;
    fn iter_between(&self, from: usize, to: usize) -> impl Iterator<Item = (usize, char)>;
}

impl Haystack for &str {
    fn try_make_contiguous(&mut self) {}

    fn is_contiguous(&self) -> bool {
        true
    }

    fn len(&self) -> usize {
        str::len(self)
    }

    fn substr_from<'a>(&'a self, offset: usize) -> Option<Cow<'a, str>> {
        if offset > self.len() {
            None
        } else {
            let raw = &self.as_bytes()[offset..];
            // SAFETY: assumes a valid byte offset
            Some(Cow::Borrowed(unsafe { std::str::from_utf8_unchecked(raw) }))
        }
    }

    fn substr<'a>(&'a self, from: usize, to: usize) -> Cow<'a, str> {
        Cow::Borrowed(&self[from..to])
    }

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        Some(
            self.char_indices()
                .take_while(|&(idx, _)| idx < byte_idx)
                .count(),
        )
    }

    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        self.char_indices().nth(char_idx).map(|(idx, _)| idx)
    }

    fn iter_from(&self, from: usize) -> Option<impl Iterator<Item = (usize, char)>> {
        if from >= self.len() {
            None
        } else {
            Some(
                self[from..]
                    .char_indices()
                    .map(move |(i, ch)| (i + from, ch)),
            )
        }
    }

    fn iter_between(&self, from: usize, to: usize) -> impl Iterator<Item = (usize, char)> {
        self[from..to]
            .char_indices()
            .map(move |(i, ch)| (i + from, ch))
    }
}

impl Haystack for GapBuffer {
    fn try_make_contiguous(&mut self) {
        self.make_contiguous();
    }

    fn is_contiguous(&self) -> bool {
        self.is_contiguous()
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn substr_from<'a>(&'a self, offset: usize) -> Option<Cow<'a, str>> {
        if offset > self.len() {
            None
        } else {
            // SAFETY: assumes make_contiguous was called first
            Some(Cow::Borrowed(unsafe { self.substr_from(offset) }))
        }
    }

    fn substr<'a>(&'a self, from: usize, to: usize) -> Cow<'a, str> {
        self.slice_from_byte_offsets(from, to).into_cow()
    }

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        if byte_idx > self.len() {
            None
        } else {
            Some(self.byte_to_char(byte_idx))
        }
    }

    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        if char_idx > self.len_chars() {
            None
        } else {
            Some(self.char_to_byte(char_idx))
        }
    }

    fn iter_from(&self, from: usize) -> Option<impl Iterator<Item = (usize, char)>> {
        if from >= self.len() {
            None
        } else {
            let mut acc = from;
            Some(
                self.slice_from_byte_offsets(from, self.len())
                    .chars()
                    .map(move |ch| {
                        let item = (acc, ch);
                        acc += ch.len_utf8();
                        item
                    }),
            )
        }
    }

    fn iter_between(&self, from: usize, to: usize) -> impl Iterator<Item = (usize, char)> {
        let mut acc = from;

        self.slice_from_byte_offsets(from, to)
            .chars()
            .map(move |ch| {
                let item = (acc, ch);
                acc += ch.len_utf8();
                item
            })
    }
}
