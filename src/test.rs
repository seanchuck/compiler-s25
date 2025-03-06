#[cfg(test)]
mod tests {
    // Include the generated tests.
    include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

    // This is your parser test function.
    // Adjust the signature and logic to call your actual parser and perform the semantic checks.
    fn run_parser_test(file: &str) -> bool {
        // For example, you might:
        // 1. Read the test file.
        // 2. Invoke your parser with the file content.
        // 3. Compare the parser’s output (or error) to the expected “legal” or “illegal” result.
        //
        // Here we use a dummy implementation that always returns true.
        //
        // Replace this with your parser invocation logic.
        use std::fs;
        let content = fs::read_to_string(file)
            .unwrap_or_else(|_| panic!("Failed to read test file: {}", file));
        
        // Call your parser function here, for example:
        // let parse_result = my_parser::parse(&content);
        // Then check if parse_result is as expected.
        
        // Dummy condition – replace with real check.
        !content.is_empty()
    }
}
