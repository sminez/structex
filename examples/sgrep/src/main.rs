use ad_regex::{CachingStream, Regex};
use anyhow::Result;
use clap::Parser;
use std::{
    collections::HashMap,
    fs,
    io::{stdin, stdout},
};
use structex::{
    Structex, StructexBuilder, TaggedCaptures,
    re::{Haystack, Writable},
    template::Template,
};

/// A simple structural expression powered grep.
///
/// Run a the given EXPRESSION against FILES, defaulting to stdin if now files are provided.
/// Matches are printed with optional templating if provided in the expression.
#[derive(Parser)]
struct Args {
    /// The stuctural regular expression to run.
    expression: String,

    /// Files to search.
    ///
    /// Defaults to stdin if no files are provided
    files: Vec<String>,
}

fn main() -> Result<()> {
    let Args { expression, files } = Args::try_parse()?;

    let se: Structex<Regex> = StructexBuilder::new(expression)
        .with_allowed_argless_tags("p")
        .with_allowed_single_arg_tags("p")
        .require_actions()
        .build()?;

    let mut templates = HashMap::new();
    for action in se.actions() {
        if let Some(arg) = action.arg() {
            let t = Template::parse(arg)?;
            templates.insert(action.id(), t);
        }
    }

    if files.is_empty() {
        run_for(&se, &CachingStream::new(stdin()), &templates, |h, caps| {
            h.clear_until(caps.from())
        })?;
    } else {
        for file in files {
            let s = fs::read_to_string(file)?;
            run_for(&se, s.as_str(), &templates, |_, _| {})?;
        }
    }

    Ok(())
}

fn run_for<'h, H>(
    se: &Structex<Regex>,
    h: &'h H,
    templates: &HashMap<usize, Template>,
    after_match: impl Fn(&'h H, &TaggedCaptures<'h, H>),
) -> Result<()>
where
    H: Haystack<Regex> + ?Sized,
{
    for caps in se.iter_tagged_captures(h) {
        let id = caps.id().unwrap();

        match templates.get(&id) {
            Some(t) => println!("{}", t.render(&caps)?),
            None => {
                caps.as_slice().write_to(&mut stdout())?;
            }
        }

        (after_match)(h, &caps);
    }

    Ok(())
}
