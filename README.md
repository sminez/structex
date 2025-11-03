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
actions that are applied to each match.

A goal of this crate is to propose a new way of working with structural regular
expressions by splitting the problem into two parts:

  1. Locating and refining matches.
  2. Applying actions to each match.

Doing so allows for a common implementation of the structural matching element
that can then be used to implement a wide range of functionality based on the
actions assigned to each match. Effectively ending up with a regex engine that
supports `flat_map` and `filter` style operations on matches as part of the
matching behaviour itself.

### Locating and refining matches

This side of the problem is effectively a subset of the original structural
regular expressions, and the primary focus of this crate, where we define a set
of operators that allow for breaking apart and filtering the current match. In
order to start matching we first set the current `dot` (selected text) to the
full input and then accept a chain of one or more of the following operators:

  - `x/$re/` "extract" and iterate over all non-overlapping matches of `$re`
    within the current dot.
  - `y/$re/` "split" the current dot using `$re` as the delimiter and iterate
    over the resulting substrings.
  - `n/$re/` "narrow"[^narrow] the current dot to the first match of `$re`
     within it.
  - `g/$re/` "guard" the rest of the following chain on `$re` matching dot.
  - `v/$re/` "inverse guard" the rest of the following chain on `$re` _not_
    matching dot.

[^narrow]: The `n` narrowing operator is a new edition to the syntax added by
this crate. In Pike's original system this narrowing was possible via "address"
expressions that set dot using a rich syntax that made use of both regular
expressions and Sam's editor state for the file being manipulated. In order to
keep the syntax consistent I have opted to make this an explicit operator and
only support narrowing the current dot to a substring rather than extending
both forward and backward (which also prevents some further issues and
restrictions within the implementation).

With these operators defined we can write expressions that combine them in
a variety of interesting ways. For example, the following expression breaks
the input into sentences delimited by `.`, keeps sentences that contain the
substring `Alice` and then narrows to the last word in the sentence as a
submatch:

```text
x/(.|\n)*?\./ g/Alice/ n/(\w+)\./
```

Running such an expression over the following text:

```text
This is a multi-line string that mentions peoples names. People like Alice
and Bob. People like Claire and David, but really we're here to talk about
Alice. Alice is everyone's friend.
```

Would extract the words `Bob`, `Alice` and `friend` in order as the final
submatch from the "narrow" expression.

That's already quite powerful and expressive but there is one more piece of
syntax that we define which allows us to do some _really_ interesting things.

We define a parallel "group" to be an ordered list of expressions that are run
over same initial dot rather than sequentially refining each match position.
To distinguish this from a normal expression chain, the group as a whole is
wrapped in curly braces and each "branch" is terminated with a semi-colon. It
is a syntax error to have an empty group or to omit the semi-colon at the end
of a branch.

An group such as the following would run each expression in parallel over the
entire input, interleving matches from each branch as they are found:
```text
{
  # if dot contains "Alice" extract the last word before a '.'
  g/Alice/ n/(\w+)\./;

  # if it doesn't, extract the first word of dot instead
  v/Alice/ n/(\w+)/;
}
```

Running this against our input from before will produce `friend`, as we are
running over the entire input.

If we add a group to the end of an expression chain then it runs for each
match produced by the chain. So, if we add back our "extract" for splitting
the input into sentences:
```text
x/(.|\n)*?\./ {
  g/Alice/ n/(\w+)\./;
  v/Alice/ n/(\w+)/;
}
```

We'll get `This` (a new match) followed by `Bob`, `Alice` and `friend` that
we had before.

This combination of guards and parallel groups allows us to conditionally
extract different substrings based on the surrounding structure, but at a
cost: we now don't know which branch matched inside of the parallel group.
To address that, we need to address the other half of the problem: applying
actions to matches.

### Applying actions

The expressions shown above have no explicit action assigned to them, so
they will result in the default action of simply emitting the match. If
we instead assign an "action" tag to the end of each branch, that action
will be emitted alongside each match coming from that branch:
```text
x/(.|\n)*?\./ {
  g/Alice/ n/(\w+)\./ A;
  v/Alice/ n/(\w+)/ B;
}
```

Now we will see an `A` tag when we match the first branch and a `B` tag
when we match the second. Running again over our input we will get both
the matches themselves _and_ their associated tag: `("This", 'B')`, 
`("Bob", 'A')`, `("Alice", 'A')` and `("friend", 'A')`.

Actions can also have an associated "argument" enclosed in slashes. This
is useful for defining tags that control the type of action to be taken
(for example `p` for printing) and then an additional argument that can
be used alongside the match in order to handle the action (such as a
template string). See the [templated_printing][7] example in the repo for
a demonstration of how this can be implemented for a more complicated
expression that uses multiple different template strings.


[0]: https://docs.rs/regex/latest/regex/index.html
[1]: http://doc.cat-v.org/plan_9/4th_edition/papers/sam/
[2]: http://doc.cat-v.org/bell_labs/structural_regexps/
[3]: https://en.wikipedia.org/wiki/Regular_expression
[4]: http://acme.cat-v.org/
[5]: https://github.com/martanne/vis
[6]: https://github.com/sminez/ad
[7]: https://github.com/sminez/structex/blob/main/examples/templated_printing.rs
