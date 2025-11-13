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

/// A simple structural expression powered sed.
///
/// Run a the given EXPRESSION against FILES, defaulting to stdin if now files are provided.
///
/// By default all input is echoed to stdout as in sed. The following structex actions are
/// supported:
///   - d: delete the matched text
///   - c: change the matched text to the output of the given template
///   - i: insert the output of the given template before the match
///   - a: insert the output of the given template after the match
#[derive(Parser)]
struct Args {
    /// The stuctural regular expression to run.
    expression: String,

    /// Files to run against.
    ///
    /// Defaults to stdin if no files are provided
    files: Vec<String>,
}

fn main() -> Result<()> {
    let Args { expression, files } = Args::try_parse()?;

    let se: Structex<Regex> = StructexBuilder::new(expression)
        .with_allowed_argless_tags("d")
        .with_allowed_single_arg_tags("aci") // typos:ignore
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

fn run_for<H>(
    se: &Structex<Regex>,
    h: H,
    templates: &HashMap<usize, Template>,
    after_match: impl Fn(H, &TaggedCaptures<H>),
) -> Result<()>
where
    H: Haystack<Regex>,
{
    let mut pos = 0;

    for caps in se.iter_tagged_captures(h) {
        let action = caps.action.as_ref().unwrap();
        let id = action.id();

        if caps.from() > pos {
            h.slice(pos..caps.from()).write_to(&mut stdout())?;
        }

        match action.tag() {
            'd' => (), // just consume the matched text
            'c' => print!("{}", templates[&id].render(&caps)?),
            'i' => {
                print!("{}", templates[&id].render(&caps)?);
                caps.as_slice().write_to(&mut stdout())?;
            }
            'a' => {
                caps.as_slice().write_to(&mut stdout())?;
                print!("{}", templates[&id].render(&caps)?);
            }

            _ => unreachable!(),
        }

        pos = caps.to();
        (after_match)(h, &caps);
    }

    if pos < h.max_len() {
        h.slice(pos..h.max_len()).write_to(&mut stdout())?;
    }

    Ok(())
}
