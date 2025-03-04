#!/bin/bash

# Define the directory containing test cases
TEST_DIR="tests/semantics"
OUTPUT_FILE="output.log"

# Clear the output file if it exists
> $OUTPUT_FILE

# Counter for tests that produce output
count=0

# Iterate over all .dcf files sorted naturally by filename
for file in $(ls -v "$TEST_DIR"/*.dcf); do
    echo "Running test: $file" | tee -a $OUTPUT_FILE

    # Run the test and capture the output
    output=$(./run.sh "$file" -t inter 2>&1)

    # Append output to log file
    echo "$output" >> $OUTPUT_FILE
    echo "---------------------------------" >> $OUTPUT_FILE

    # If output is not empty, increment counter
    if [[ -n "$output" ]]; then
        ((count++))
    fi
done

echo "All tests completed. Results saved in $OUTPUT_FILE"
echo "Number of tests that produced output: $count"
