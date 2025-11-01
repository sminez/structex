//! An example of how to use a Structex alongside the handlebars crate to support simple string
//! templating for a 'p' print action.
use handlebars::Handlebars;
use regex::Regex;
use std::collections::HashMap;
use structex::{Action, Structex};

const SE: &str = r#"
x/^impl(?:<.*?>)?.*? (\w+)(?:.|\n)*?^\}/
v/^impl(?:<.*?>)?.*? for/ {

  p/\nimpl {{$1}}/;

  x/fn(?:.|\n)*?\{/ {
    g/->/ x/fn (\w+)(?:.|\n)*?-> (.*?)\w*\{/ {
      g/&('. )?mut self/ p/  mut {{$1}} -> {{$2}}/;
      v/&('. )?mut self/ p/      {{$1}} -> {{$2}}/;
    };

    v/->/ x/fn (\w+)(?:.|\n)*\{/ {
      g/&('. )?mut self/ p/  mut {{$1}} -> ()/;
      v/&('. )?mut self/ p/      {{$1}} -> ()/;
    };

  };
}
"#;

fn main() {
    // Compile the structex
    let se: Structex<Regex> = Structex::compile(SE).unwrap();

    // Create a new handlebars registry and register each print template that was located by the
    // Structex during compilation.
    let mut reg = Handlebars::new();
    reg.register_escape_fn(|s| s.to_string());

    for template in se.action_content() {
        reg.register_template_string(template, template).unwrap();
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

                println!(
                    "{}",
                    reg.render(template, &ctx).unwrap().replace("\\n", "\n")
                );
            }

            // If we somehow end up receiving an unknown template tag then panic
            Some(action) => panic!("unknown action: {}", action.tag),
        }
    }
}
