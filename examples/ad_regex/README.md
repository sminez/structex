# ad-regex

This is a modified version of the regex engine from the [ad][0] text editor
along with its gap buffer implementation for use as a haystack.

It is used in examples where we need to work with streaming inputs. It uses
its own regex syntax that supports `@` as a "true any" pattern that matches
any input character, including newlines. It is _not_ PCRE compatible and it
is certainly not anywhere near as performant as the `regex` crate.

In short, this is here for demonstration purposes: you almost certainly
don't want to make use of it outside of these examples or `ad` itself, where
its primary purpose is to support editing expressions within the editor.

[0]: https://github.com/sminez/ad
