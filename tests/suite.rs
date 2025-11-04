use regex::Regex;
use simple_test_case::dir_cases;
use simple_txtar::Archive;
use std::collections::HashMap;
use structex::{Structex, StructexBuilder};
use tinytemplate::TinyTemplate;

fn parse_case<T>(arr: &Archive, f: impl Fn(String) -> T) -> (&str, &str, Vec<T>) {
    let se = &arr.get("se").unwrap().content;
    let haystack = &arr.get("haystack").as_ref().unwrap().content;
    let expected: Vec<T> = arr
        .get("matches")
        .unwrap()
        .content
        .trim_end()
        .lines()
        .map(|s| s.replace("\\n", "\n"))
        .map(f)
        .collect();

    (se, haystack, expected)
}

#[dir_cases("tests/data/basic")]
#[test]
fn basic(_path: &str, content: &str) {
    let arr = Archive::from(content);
    let (se, haystack, expected_matches) = parse_case(&arr, |s| s);
    let se: Structex<Regex> = Structex::new(se).unwrap();
    let matches: Vec<String> = se
        .iter_tagged_captures(haystack)
        .map(|m| m.as_slice().to_string())
        .collect();

    assert_eq!(matches, expected_matches);
}

#[dir_cases("tests/data/tagged_matches")]
#[test]
fn tagged_matches(_path: &str, content: &str) {
    let arr = Archive::from(content);
    let (se, haystack, expected_matches) = parse_case(&arr, |s| {
        let (tag, s) = s.split_once(' ').unwrap();
        (tag.chars().next().unwrap(), s.to_string())
    });
    let se: Structex<Regex> = Structex::new(se).unwrap();
    let matches: Vec<(char, String)> = se
        .iter_tagged_captures(haystack)
        .map(|m| (m.tag().unwrap(), m.as_slice().to_string()))
        .collect();

    assert_eq!(matches, expected_matches);
}

#[dir_cases("tests/data/template_printing")]
#[test]
fn template_printing(_path: &str, content: &str) {
    let arr = Archive::from(content);
    let (se, haystack, expected_output) = parse_case(&arr, |s| s);
    let se: Structex<Regex> = StructexBuilder::new(se)
        .with_allowed_argless_tags("")
        .with_allowed_single_arg_tags("p")
        .build()
        .unwrap();

    // Parse and register the templates
    let mut tt = TinyTemplate::new();
    tt.set_default_formatter(&tinytemplate::format_unescaped);
    for action in se.actions() {
        if let Some(template) = action.arg.as_ref() {
            tt.add_template(template, template).unwrap();
        }
    }

    let output: Vec<String> = se
        .iter_tagged_captures(haystack)
        .map(|m| {
            let template = m.arg().unwrap();
            let ctx: HashMap<usize, Option<&str>> = m.iter_submatches().enumerate().collect();
            tt.render(template, &ctx).unwrap()
        })
        .collect();

    assert_eq!(output, expected_output);
}
