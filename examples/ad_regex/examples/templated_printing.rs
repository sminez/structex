//! The same example as the main templated_printing.rs but using the ad regex engine and syntax.
use ad_regex::{Regex, gap_buffer::GapBuffer};
use structex::{Structex, StructexBuilder, template::Template};

const SE: &str = r#"
x/^impl(?:<.*?>)?.*? (\w+)(?:.|\n)*?^\}/
v/^impl(?:<.*?>)?.*? for/ {

  p/\nimpl {1}/;

  x/fn@*?\{/ {
    g/->/ n/fn (\w+)@*?-> (.*?)\w*\{/ {
      g/(&?('. ))?mut self/ p/  mut {1} -> {2}/;
      v/(&?('. ))?mut self/ p/      {1} -> {2}/;
    };

    v/->/ n/fn (\w+)@*\{/ {
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

    let templates: Vec<Template> = se
        .actions()
        .iter()
        .map(|action| Template::parse(action.arg().unwrap()).unwrap())
        .collect();

    // Match against the ast.rs file from this crate
    let haystack = include_str!("../src/ast.rs");

    for caps in se.iter_tagged_captures(&GapBuffer::from(haystack)) {
        // We know we only have 'p' actions with an argument from the compilation config we set
        // above, so we know that each captures has an action and that the action ID is a valid
        // index into our vec of templates.
        let id = caps.id().unwrap();
        println!("{}", templates[id].render(&caps).unwrap());
    }
}
