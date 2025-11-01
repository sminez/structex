use regex::Regex;
use structex::Structex;

const SE: &str = r#"
x/fn(?:.|\n)*?\{/                       # select all Rust function signatures up to the opening brace
g/->.*Result.*\{/                # keep those that return some form of Result
x/fn (\w+)(?:.|\n)*?-> (.*?) \{/        # extract the function name and return type from the signature
"#;

fn main() {
    let se: Structex<Regex> = Structex::compile(SE).unwrap();
    let haystack = include_str!("../src/ast.rs");

    for m in se.iter_matches(haystack) {
        let fn_name = m.submatch_text(haystack, 1).unwrap();
        let ret_ty = m.submatch_text(haystack, 2).unwrap();
        println!(
            "{}:{} ({:?}) {fn_name}: {ret_ty}",
            m.from(),
            m.to(),
            m.action
        );
    }
}
