#!/bin/bash

# Define the directory containing test cases
TEST_DIR="tests/semantics"
OUTPUT_FILE="output.log"

# Clear the output file if it exists
> $OUTPUT_FILE

# Iterate over all .dcf files sorted naturally by filename
for file in $(ls -v "$TEST_DIR"/*.dcf); do
    echo "Running test: $file" | tee -a $OUTPUT_FILE
    ./run.sh "$file" -t inter >> $OUTPUT_FILE 2>&1
    echo "---------------------------------" >> $OUTPUT_FILE
done

echo "All tests completed. Results saved in $OUTPUT_FILE"
