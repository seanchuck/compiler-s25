use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

fn main() {
    // Output file for generated tests
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("generated_tests.rs");
    let mut f = File::create(&dest_path).unwrap();

    // Directory containing your semantic test files
    let test_dir = "tests/semantics";

    // Collect all .dcf files in the directory, then sort them
    let mut test_files: Vec<_> = fs::read_dir(test_dir)
        .expect("Unable to read tests/semantics directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()?.to_str()? == "dcf" {
                Some(path)
            } else {
                None
            }
        })
        .collect();

    // Sort files (you can adjust the sorting if needed)
    test_files.sort();

    // Generate a test function for each file.
    for test_file in test_files {
        // Create a valid Rust identifier from the file stem (you may need to adjust for illegal characters)
        let file_stem = test_file.file_stem().unwrap().to_str().unwrap();
        let test_name = file_stem.replace("-", "_"); // simple conversion

        // Write out a test function that calls your parser test function
        writeln!(
            f,
            "#[test]
fn test_{name}() {{
    let test_file = \"{file}\";
    let result = run_parser_test(test_file);
    assert!(result, \"Parser result for {{}} did not match expectation\", test_file);
}}",
            name = test_name,
            file = test_file.display()
        )
        .unwrap();
    }

    // Tell Cargo to re-run this script if anything in the test directory changes.
    println!("cargo:rerun-if-changed=tests/semantics");
}
