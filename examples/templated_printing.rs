//! An example of how to use a Structex alongside the tinytemplate crate to support simple string
//! templating for a 'p' print action.
use regex::Regex;
use std::collections::HashMap;
use structex::{Structex, StructexBuilder};
use tinytemplate::TinyTemplate;

// This is a much more complicated, compound expression. It is searching for Rust "impl" blocks
// within a file, filtering out those that look like a trait implementation ("impl X for Y") before
// running a parallel group that breaks the impl block into non-overlapping sections:
//   - First the name of the type that owns the impl block is printed
//   - Then, a second extract is run to locate all method signatures (up to the opening '{')
//   - For each signature, a second nested parallel group is run:
//     - If the signature includes "->" we assume it has a return type and we attempt to narrow the
//       matching substring to locate both the method name and return type
//     - If the signature doesn't include "->", we only narrow and look for the function name.
//   - Then, for each signature we run one final parallel group that checks to see if "self" is
//     mutable and uses all of that information to print out a summary for the method.
//     - The template strings used for each branch are slight modifications of one another
//       depending on which branch has been taken.
const SE: &str = r#"
x/^impl(?:<.*?>)?.*? (\w+)(?:.|\n)*?^\}/
v/^impl(?:<.*?>)?.*? for/ {

  p/\nimpl {1}/;

  x/fn(?:.|\n)*?\{/ {
    g/->/ n/fn (\w+)(?:.|\n)*?-> (.*?)\w*\{/ {
      g/(&?('. ))?mut self/ p/  mut {1} -> {2}/;
      v/(&?('. ))?mut self/ p/      {1} -> {2}/;
    };

    v/->/ n/fn (\w+)(?:.|\n)*\{/ {
      g/(&?('. ))?mut self/ p/  mut {1} -> ()/;
      v/(&?('. ))?mut self/ p/      {1} -> ()/;
    };

  };
}
"#;

fn main() {
    // Compile the structex only allowing a 'p' tag that accepts a single argument
    let se: Structex<Regex> = StructexBuilder::new(SE)
        .with_allowed_argless_tags("")
        .with_allowed_single_arg_tags("p")
        .build()
        .unwrap();

    // Create a new template registry and register each print template that was located by the
    // Structex during compilation.
    let mut tt = TinyTemplate::new();
    tt.set_default_formatter(&tinytemplate::format_unescaped);

    for action in se.actions() {
        if let Some(template) = action.arg.as_ref() {
            tt.add_template(template, template).unwrap();
        }
    }

    // Match against the ast.rs file from this crate
    let haystack = include_str!("../src/ast.rs");

    for m in se.iter_tagged_captures(haystack) {
        // We know we only have 'p' actions with an argument from the compilation config we set
        // above, so we can safely unwrap the match argument for use as our template.
        let template = m.arg().unwrap();
        let ctx: HashMap<usize, Option<&str>> = m.iter_submatches().enumerate().collect();
        println!("{}", tt.render(template, &ctx).unwrap());
    }
}
