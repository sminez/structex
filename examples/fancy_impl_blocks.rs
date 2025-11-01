//! An example of how to use a Structex alongside the tinytemplate crate to support simple string
//! templating for a 'p' print action.
use regex::Regex;
use std::collections::HashMap;
use structex::{Action, Structex, StructexBuilder};
use tinytemplate::TinyTemplate;

const SE: &str = r#"
x/^impl(?:<.*?>)?.*? (\w+)(?:.|\n)*?^\}/
v/^impl(?:<.*?>)?.*? for/ {

  p/\nimpl {$1}/;

  x/fn(?:.|\n)*?\{/ {
    g/->/ x/fn (\w+)(?:.|\n)*?-> (.*?)\w*\{/ {
      g/(&?('. ))?mut self/ p/  mut {$1} -> {$2}/;
      v/(&?('. ))?mut self/ p/      {$1} -> {$2}/;
    };

    v/->/ x/fn (\w+)(?:.|\n)*\{/ {
      g/(&?('. ))?mut self/ p/  mut {$1} -> ()/;
      v/(&?('. ))?mut self/ p/      {$1} -> ()/;
    };

  };
}
"#;

fn main() {
    // Compile the structex only allowing a 'p' tag that accepts a single argument
    let se: Structex<Regex> = StructexBuilder::default()
        .with_allowed_argless_tags("")
        .with_allowed_single_arg_tags("p")
        .build(SE)
        .unwrap();

    // Create a new template registry and register each print template that was located by the
    // Structex during compilation.
    let mut tt = TinyTemplate::new();
    tt.set_default_formatter(&tinytemplate::format_unescaped);

    for template in se.action_content() {
        tt.add_template(template, template).unwrap();
    }

    // Match against the ast.rs file from this crate
    let haystack = include_str!("../src/ast.rs");

    for m in se.iter_matches(haystack) {
        // If we have no action or no template then just print the full match
        match m.action {
            None
            | Some(Action {
                tag: 'p',
                content: None,
            }) => {
                println!("{}", m.match_text(haystack));
            }

            // If we have a template, build a templating context from the extracted matches and
            // render the template
            Some(Action {
                tag: 'p',
                content: Some(ref template),
            }) => {
                let ctx: HashMap<String, String> = m
                    .iter_caps(haystack)
                    .enumerate()
                    .map(|(i, s)| (format!("${i}"), s.unwrap_or_default().to_string()))
                    .collect();

                println!("{}", tt.render(template, &ctx).unwrap());
            }

            // If we somehow end up receiving an unknown template tag then panic
            Some(action) => panic!("unknown action: {}", action.tag),
        }
    }
}
