# Structex - structural regular expressions for Rust

This crate provides a generic [structural regular expression][0] engine that can be backed
by a user provided regular expression engine. Support for using the [regex][1] crate is
provided from this crate under the `regex` feature.

### WIP

This crate is still very much a work in progress. Error reporting needs to be improved,
testing needs to be completed and public APIs need to be documented. There is also a
fair amount of helper functionality that should be added.

[0]: http://doc.cat-v.org/bell_labs/structural_regexps/
[1]: https://docs.rs/regex/latest/regex/index.html
