#!/bin/bash

# Define the directory containing test cases
TEST_DIR="tests/semantics"
OUTPUT_FILE="output.log"

# Clear the output file if it exists
> "$OUTPUT_FILE"

# Counter for tests that produce output
count=0

# Find all test files, extract rule numbers, and sort them numerically
sorted_files=$(ls "$TEST_DIR"/*.dcf | sort -t'/' -k3,3V -k2,2n)

# Iterate over sorted test cases
for file in $sorted_files; do
    echo "Running test: $file" | tee -a "$OUTPUT_FILE"

    # Run the test and capture the output
    output=$(./run.sh "$file" -t inter 2>&1)

    # Append output to log file
    echo "$output" >> "$OUTPUT_FILE"
    echo "---------------------------------" >> "$OUTPUT_FILE"

    # If output is not empty, increment counter
    if [[ -n "$output" ]]; then
        ((count++))
    fi
done

echo "All tests completed. Results saved in $OUTPUT_FILE"
echo "Number of tests that produced output: $count"
