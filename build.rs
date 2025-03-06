use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

fn main() {
    // Where to write the generated test functions.
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("generated_tests.rs");
    let mut f = File::create(&dest_path).expect("Could not create generated_tests.rs");

    // Directory containing semantic test files.
    let test_dir = "tests/semantics";

    // Collect all .dcf files from the directory.
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

    // Sort the test files (adjust sorting if needed).
    test_files.sort();

    // Generate one test function per file.
    for test_file in test_files {
        // Create a valid Rust identifier from the file stem.
        let file_stem = test_file.file_stem().unwrap().to_str().unwrap();
        let test_name = file_stem.replace("-", "_"); // Replace dashes with underscores.

        writeln!(
            f,
            "#[test]
fn test_{name}() {{
    let test_file = \"{file}\";
    let result = crate::tests::run_parser_test(test_file);
    assert!(result, \"Parser result for {{}} did not match expectation\", test_file);
}}",
            name = test_name,
            file = test_file.display()
        )
        .expect("Failed to write test function");
    }

    // Instruct Cargo to re-run this build script if any file in tests/semantics changes.
    println!("cargo:rerun-if-changed=tests/semantics");
}
