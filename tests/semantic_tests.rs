#[cfg(test)]
mod tests {
    use std::process::Command;
    use std::str;

    // Include the generated tests from the build script.
    include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

    /// Runs the parser on the provided file and checks whether the exit code
    /// matches the expected outcome.
    ///
    /// Expected behavior:
    /// - Files *without* "illegal" in their name should have the parser exit with code 0.
    /// - Files containing "illegal" should have the parser exit with a non-zero code.
    pub fn run_parser_test(file: &str) -> bool {
        // Execute the parser command (mirroring your shell script).
        let output = Command::new("./run.sh")
            .arg(file)
            .arg("-t")
            .arg("inter")
            .output()
            .unwrap_or_else(|e| panic!("Failed to execute parser on {}: {}", file, e));

        // Log output if there is any (helpful for debugging).
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        if !stdout.is_empty() || !stderr.is_empty() {
            eprintln!(
                "Running test: {}\n--- STDOUT ---\n{}\n--- STDERR ---\n{}",
                file, stdout, stderr
            );
        }

        // Determine what the expected outcome is based on the filename.
        let expected_success = !file.contains("illegal");

        // Compare the parser's exit status with the expected outcome.
        if output.status.success() == expected_success {
            true
        } else {
            if expected_success {
                eprintln!(
                    "Test file '{}' was expected to be legal, but the parser exited with an error (exit status: {:?}).",
                    file, output.status.code()
                );
            } else {
                eprintln!(
                    "Test file '{}' was expected to be illegal, but the parser exited successfully.",
                    file
                );
            }
            false
        }
    }
}
