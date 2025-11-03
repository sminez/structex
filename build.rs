fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "integration_test")]
    {
        use std::fs;
        const DELIMITER: &str = "\n\n-----\n\n";

        // Break our test case files into individual files that can be passed to dir_cases
        _ = fs::create_dir("tests/data");

        for file in fs::read_dir("tests/raw_data")? {
            let path = file.unwrap().path();
            let dir = format!("tests/data/{}", path.file_stem().unwrap().display());
            _ = fs::create_dir(&dir);

            let raw = fs::read_to_string(&path).unwrap();
            for case in raw.split(DELIMITER) {
                let name_pos = case
                    .lines()
                    .position(|line| line == "-- name --")
                    .unwrap_or_else(|| panic!("no name in {case:?}"));
                let name = case.lines().nth(name_pos + 1).unwrap().replace(' ', "_");
                fs::write(format!("{dir}/{name}.txtar"), case).unwrap();
            }
        }
    }

    Ok(())
}
