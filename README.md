# Structex - structural regular expressions for Rust

[![Build](https://github.com/sminez/structex/workflows/Build/badge.svg)](https://github.com/sminez/structex/actions?query=workflow%3ABuild)
[![crates.io version](https://img.shields.io/crates/v/structex)](https://crates.io/crates/structex)
[![docs.rs](https://img.shields.io/docsrs/structex?logo=rust)](https://docs.rs/structex)

This crate provides a generic structural regular expression engine that can be
backed by a user provided regular expression engine. Support for using the
[regex][0] crate is provided from this crate under the `regex` feature.

## Documentation

This crate's public API is documented in [docs.rs](https://docs.rs/structex).

## Usage

To add this crate to an existing Rust project, either add `structex` to your
`Cargo.toml` or run `cargo add structex`. By default, support for using the
[regex][0] crate as a back end is enabled via the `regex` feature. You can
opt out of this by disabling default features.

The following is an example of how to find Rust functions that return Result
types from a given source file:

```rust
use regex::Regex;
use structex::Structex;

// Compile the structex using the regex crate as the underlying engine.
let se: Structex<Regex> = Structex::new(r#"
  x/fn(?:.|\n)*?\{/                   # select all Rust function signatures up to the opening brace
  g/->.*Result.*\{/                   # keep those that return some form of Result
  n/fn (\w+)(?:.|\n)*?-> (.*?) \{/    # extract the function name and return type from the signature
"#).unwrap();

// Match against the ast.rs rust file from this crate
let haystack = include_str!("src/ast.rs");

// Print out the two capture groups along with the byte offsets of the match position.
for m in se.iter_tagged_captures(haystack) {
    let fn_name = m.submatch_text(1).unwrap();
    let ret_ty = m.submatch_text(2).unwrap();
    println!("{}:{} {fn_name}: {ret_ty}", m.from(), m.to(),);
}
```

## What are structural regular expressions?

Introduced by Rob Pike in his [Sam][1] text editor and discussed in his [1987
paper][2], structural regular expressions provide a notation and semantics for
composing [regular expressions][3] in order to more easily describe the
_structure_ of the text being searched. The syntax used by Pike in `Sam`
combined looping constructs and conditional expressions with printing and
editing actions such as deleting the matched text, inserting before or after
the match and replacing the match entirely.

There have been a handful of other implementations over the years, notably
Pike's later editor [acme][4], [vis][5] and my own editor [ad][6], all of which
follow the original approach of coupling the composition operators with editing
actions that are applied to each match. A goal of this crate is propose a new
way of working with structural regular expressions by splitting the problem into
two parts:

  1. Locating and refining matches.
  2. Applying actions to each match.

Doing so allows for a common implementation of the structural matching element
that can then be used to implement a wide range of functionality based on the
actions assigned to each match.

[0]: https://docs.rs/regex/latest/regex/index.html
[1]: http://doc.cat-v.org/plan_9/4th_edition/papers/sam/
[2]: http://doc.cat-v.org/bell_labs/structural_regexps/
[3]: https://en.wikipedia.org/wiki/Regular_expression
[4]: http://acme.cat-v.org/
[5]: https://github.com/martanne/vis
[6]: https://github.com/sminez/ad
