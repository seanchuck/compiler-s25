#!/bin/bash


# Runs the semantic checker on all legal parser test files.
# Not all should pass since some (about half) are semantically
# invalid. So be sure to check the results manually!

# Set the base directory for valid parser tests
BASE_DIR="public-tests/phase1-parser/public/legal"

# Define the output file
OUTPUT_FILE="symtable_test_results.out"

# Clear the output file if it exists
> "$OUTPUT_FILE"

# Counters for test results
correct=0
incorrect=0

# Loop through all valid Decaf files in the legal folder
for file in "$BASE_DIR"/*; do
    if [[ -f "$file" ]]; then
        echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
        echo "Running: ../run.sh $file -t inter" | tee -a "$OUTPUT_FILE"
        echo "----------------------------------------" | tee -a "$OUTPUT_FILE"
        
        # Run the script and capture error status
        ERROR_OUTPUT=$(./run.sh "$file" -t inter 2>&1)
        EXIT_CODE=$?

        # Save output of the command to file
        echo "$ERROR_OUTPUT" >> "$OUTPUT_FILE"

        # Check if the test passed (no errors)
        if [[ $EXIT_CODE -eq 0 ]]; then
            echo -e "PASS: $file\n" | tee -a "$OUTPUT_FILE"
            ((correct++))
        else
            echo -e "FAIL: Unexpected error occurred. ($file)\n" | tee -a "$OUTPUT_FILE"
            echo -e "Error details:\n$ERROR_OUTPUT\n" | tee -a "$OUTPUT_FILE"
            ((incorrect++))
        fi
    fi
done

# Print summary
echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
echo "Execution completed. Results saved in $OUTPUT_FILE." | tee -a "$OUTPUT_FILE"
echo "Total Passed: $correct" | tee -a "$OUTPUT_FILE"
echo "Total Failed: $incorrect" | tee -a "$OUTPUT_FILE"

# Exit with an error code if any tests failed
if [[ $incorrect -gt 0 ]]; then
    exit 1
else
    exit 0
fi
