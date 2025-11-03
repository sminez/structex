use regex::Regex;
use simple_test_case::dir_cases;
use simple_txtar::Archive;
use structex::Structex;

#[dir_cases("tests/data/basic")]
#[test]
fn basic(_path: &str, content: &str) {
    let arr = Archive::from(content);
    let se = &arr.get("se").as_ref().unwrap().content;
    let haystack = &arr.get("haystack").as_ref().unwrap().content;
    let expected_matches: Vec<String> = arr
        .get("matches")
        .unwrap()
        .content
        .trim_end()
        .lines()
        .map(|s| s.replace("\\n", "\n"))
        .collect();

    let se: Structex<Regex> = Structex::new(se).unwrap();
    let matches: Vec<String> = se
        .iter_tagged_captures(haystack)
        .map(|m| m.as_str().to_string())
        .collect();

    assert_eq!(matches, expected_matches);
}
