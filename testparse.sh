#!/bin/bash

# Set the base directory
BASE_DIR="public-tests/phase1-parser/public"

# Define the output file
OUTPUT_FILE="out.out"

# Clear the output file if it exists
> "$OUTPUT_FILE"

# Loop through all files in 'illegal' and 'legal' directories
# for dir in "illegal" "legal"; do
for dir in "illegal" "legal"; do
    for file in "$BASE_DIR/$dir"/*; do
        if [[ -f "$file" ]]; then
            echo "Running: ./run.sh $file -t parse" | tee -a "$OUTPUT_FILE"
            ./run.sh "$file" -t parse >> "$OUTPUT_FILE" 2>&1
        fi
    done
done

echo "Execution completed. Results saved in $OUTPUT_FILE."
