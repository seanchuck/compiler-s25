#!/bin/bash

# Set the base directory
BASE_DIR="tests/phase3"
ERROR_DIR="error"

# Define the output file
OUTPUT_FILE="error_only.out"

# Clear the output file if it exists
> "$OUTPUT_FILE"

correct=0
incorrect=0

for file in "$BASE_DIR/$ERROR_DIR"/err*; do
    if [[ -f "$file" ]]; then
        filename=$(basename -- "$file")
        assembly_file="$file.s"
        executable_file="$file.out"

        echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
        echo "Testing (should error): $file" | tee -a "$OUTPUT_FILE"
        echo "----------------------------------------" | tee -a "$OUTPUT_FILE"

        # Try to generate assembly
        ./run.sh -t assembly "$file" -o "$assembly_file"
        exit_code=$?

        # If the compiler itself fails, that's fine (expected error)
        if [[ $exit_code -ne 0 ]]; then
            echo "Compilation failed as expected for $file" | tee -a "$OUTPUT_FILE"
            correct=$((correct + 1))
            rm -f "$assembly_file"
            continue
        fi

        # If it compiles, try to assemble and link
        gcc -O0 -no-pie "$assembly_file" -o "$executable_file"
        if [[ $? -ne 0 ]]; then
            echo "Linking failed as expected for $file" | tee -a "$OUTPUT_FILE"
            correct=$((correct + 1))
            rm -f "$assembly_file" "$executable_file"
            continue
        fi

        # If it links, try to run it (should crash or return non-zero)
        "$executable_file"
        exit_code=$?
        if [[ $exit_code -ne 0 ]]; then
            echo "Runtime failed as expected for $file" | tee -a "$OUTPUT_FILE"
            correct=$((correct + 1))
        else
            echo "Incorrectly succeeded at runtime for $file" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
        fi

        rm -f "$assembly_file" "$executable_file"
    fi
done

echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
echo "ERROR TEST SUMMARY" | tee -a "$OUTPUT_FILE"
echo "Total Passed (correct errors): $correct" | tee -a "$OUTPUT_FILE"
echo "Total Failed (unexpected successes): $incorrect" | tee -a "$OUTPUT_FILE"

if [[ $incorrect -gt 0 ]]; then
    exit 1
else
    exit 0
fi
