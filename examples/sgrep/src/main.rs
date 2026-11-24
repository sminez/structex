use anyhow::Result;
use clap::Parser;
use std::{
    collections::HashMap,
    fs,
    io::{self, Write, stdin, stdout},
};
use structex::{
    Structex, StructexBuilder, TaggedCaptures,
    re::{Haystack, Writable},
    regex_cursor::{CachingStream, Regex},
    template::{Context, Template},
};

/// A simple structural expression powered grep.
///
/// Run a the given EXPRESSION against FILES, defaulting to stdin if now files are provided.
/// Matches are printed with optional templating if provided in the expression.
#[derive(Parser)]
struct Args {
    /// The structural regular expression to run.
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
        let h = CachingStream::new(stdin());

        run_for(&se, &h, "STDIN", &templates, |h, caps| {
            h.drop_before(caps);

            if h.has_error() {
                Err(h.unwrap_error())
            } else {
                Ok(())
            }
        })?;
    } else {
        for file in files.iter() {
            let s = fs::read_to_string(file)?;
            run_for(&se, s.as_str(), file, &templates, |_, _| Ok(()))?;
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct Ctx<'a> {
    fname: &'a str,
}

impl<'a> Context for Ctx<'a> {
    fn render_var<W>(&self, var: &str, w: &mut W) -> Option<io::Result<usize>>
    where
        W: Write,
    {
        if var == "FILENAME" {
            Some(w.write(self.fname.as_bytes()))
        } else {
            None
        }
    }
}

fn run_for<'h, H>(
    se: &Structex<Regex>,
    h: &'h H,
    fname: &str,
    templates: &HashMap<usize, Template>,
    after_match: impl Fn(&'h H, &TaggedCaptures<'h, H>) -> io::Result<()>,
) -> Result<()>
where
    H: Haystack<Regex> + ?Sized,
{
    let ctx = Ctx { fname };

    for caps in se.iter_tagged_captures(h) {
        let id = caps.id().unwrap();

        match templates.get(&id) {
            Some(t) => println!("{}", t.render_with_context(&caps, &ctx)?),
            None => {
                caps.as_slice().write_to(&mut stdout())?;
            }
        }

        (after_match)(h, &caps)?;
    }

    Ok(())
}
