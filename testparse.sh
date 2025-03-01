#!/bin/bash

# Set the base directory
BASE_DIR="public-tests/phase1-parser/public"

# Define the output file
OUTPUT_FILE="out.out"

# Clear the output file if it exists
> "$OUTPUT_FILE"

# Counters for test results
correct=0
incorrect=0

# Loop through all files in 'illegal' and 'legal' directories
for dir in "illegal" "legal"; do
    for file in "$BASE_DIR/$dir"/*; do
        if [[ -f "$file" ]]; then
            echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
            echo "Running: ./run.sh $file -t parse" | tee -a "$OUTPUT_FILE"
            echo "----------------------------------------" | tee -a "$OUTPUT_FILE"
            
            # Run the script and capture error status
            ./run.sh "$file" -t parse >> "$OUTPUT_FILE" 2>&1
            EXIT_CODE=$?

            # Check if the expectation matches the actual result
            if [[ "$dir" == "illegal" && $EXIT_CODE -eq 0 ]]; then
                echo -e "FAIL: Expected error, but none occurred. ($file)\n" | tee -a "$OUTPUT_FILE"
                ((incorrect++))
            elif [[ "$dir" == "legal" && $EXIT_CODE -ne 0 ]]; then
                echo -e "FAIL: Unexpected error occurred. ($file)\n" | tee -a "$OUTPUT_FILE"
                ((incorrect++))
            else
                echo -e "PASS: $file\n" | tee -a "$OUTPUT_FILE"
                ((correct++))
            fi
        fi
    done
done

# Print summary
echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
echo "Execution completed. Results saved in $OUTPUT_FILE." | tee -a "$OUTPUT_FILE"
echo "Total Correct: $correct" | tee -a "$OUTPUT_FILE"
echo "Total Incorrect: $incorrect" | tee -a "$OUTPUT_FILE"
