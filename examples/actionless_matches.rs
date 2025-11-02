//! A simple example of how to use Structex to print out filtered matches without an explicit
//! action.
use regex::Regex;
use structex::Structex;

const SE: &str = r#"
x/fn(?:.|\n)*?\{/                   # select all Rust function signatures up to the opening brace
g/->.*Result.*\{/                   # keep those that return some form of Result
n/fn (\w+)(?:.|\n)*?-> (.*?) \{/    # extract the function name and return type from the signature
"#;

fn main() {
    // Compile the structex using the regex crate as the underlying engine.
    let se: Structex<Regex> = Structex::new(SE).unwrap();

    // Match against the ast.rs rust file from this crate
    let haystack = include_str!("../src/ast.rs");

    // For each match, print out the two capture groups along with the byte offsets of the match
    // position and action (which should always be None).
    for m in se.iter_tagged_captures(haystack) {
        let fn_name = m.submatch_text(1).unwrap();
        let ret_ty = m.submatch_text(2).unwrap();
        println!("{}:{} {fn_name}: {ret_ty}", m.from(), m.to(),);
    }
}
