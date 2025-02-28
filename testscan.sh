#!/bin/bash
# Locally runs all tests in /public-tests and compares results to expected output.

INPUT_DIR="public-tests/phase1-scanner/public/input"
OUTPUT_DIR="public-tests/phase1-scanner/public/output"

# Temporary output file
TEMP_OUTPUT="out.out"

echo "Starting test case checks..."

# Ensure input directory exists
if [ ! -d "$INPUT_DIR" ]; then
    echo "[ERROR] Input directory '$INPUT_DIR' does not exist!"
    exit 1
fi

# Ensure output directory exists
if [ ! -d "$OUTPUT_DIR" ]; then
    echo "[ERROR] Output directory '$OUTPUT_DIR' does not exist!"
    exit 1
fi

# Counters for summary
total_tests=0
passed_tests=0
failed_tests=0

# Loop through all .dcf files in the input directory
for input_file in "$INPUT_DIR"/*.dcf; do
    # Check if any .dcf files exist, otherwise skip
    if [ ! -e "$input_file" ]; then
        echo "[ERROR] No .dcf files found in '$INPUT_DIR'."
        exit 1
    fi

    # Extract the base name (without directory and extension)
    base_name=$(basename "$input_file" .dcf)
    
    # Check if the filename contains "invalid" (case-insensitive)
    if [[ "$base_name" =~ [Ii][Nn][Vv][Aa][Ll][Ii][Dd] ]]; then
        expect_error=true
    else
        expect_error=false
    fi

    # Define expected output file
    expected_output_file="$OUTPUT_DIR/$base_name.out"
    
    # Check if the expected output file exists
    if [ ! -f "$expected_output_file" ]; then
        echo "[WARNING] Expected output file '$expected_output_file' not found for '$base_name', skipping..."
        continue
    fi

    # Increment total tests count
    ((total_tests++))

    # Print checking message before running the test case
    echo "Checking $base_name..."

    # Run the command and capture output
    ./run.sh "$input_file" -t scan --debug > "$TEMP_OUTPUT"
    exit_code=$?

  # Validate based on expected behavior
    if $expect_error; then
        if [ $exit_code -ne 0 ]; then
            echo -e "[PASS] $base_name (expected failure)\n"
            ((passed_tests++))
        else
            echo -e "[FAIL] $base_name (expected failure but passed unexpectedly!)\n"
            ((failed_tests++))
        fi
    else
        if diff -u "$TEMP_OUTPUT" "$expected_output_file" > /dev/null; then
            echo -e "[PASS] $base_name\n"
            ((passed_tests++))
        else
            echo -e "[FAIL] $base_name - Output mismatch!\n"
            diff -u "$TEMP_OUTPUT" "$expected_output_file" | head -20  # Show first 20 lines of difference
            ((failed_tests++))
        fi
    fi
done


# Cleanup
rm -f "$TEMP_OUTPUT"

# Final Summary
echo "---------------------------------"
echo "Test Summary:"
echo "Total Tests: $total_tests"
echo "Passed: $passed_tests"
echo "Failed: $failed_tests"

# Exit with a non-zero status if there were failures
if [ "$failed_tests" -gt 0 ]; then
    exit 1
else
    exit 0
fi
