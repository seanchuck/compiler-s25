#!/bin/bash
# Use the same -O flags as the run.sh script to turn on/off optimizations

# Set the base directory
BASE_DIR="tests/phase3"
INPUT_DIR="input"
OUTPUT_DIR="output"
ERROR_DIR="error"

# Define the output file
OUTPUT_FILE="out.out"

# Clear the output file if it exists
> "$OUTPUT_FILE"

# Default optimization flag
OPT_FLAGS="-O all"

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -O|--opt)
            OPT_FLAGS="-O $2"
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            exit 1
            ;;
    esac
done

# Counters for test results
correct=0
incorrect=0

# Global optimization counters
total_cp=0
total_dce=0
total_cse=0

# Function to compare outputs
compare_outputs() {
    diff -q "$1" "$2" > /dev/null
    return $?
}

# Function to count optimizations per test case
count_optimizations() {
    local debug_output="$1"
    local cp_count dce_count cse_count

    cp_count=$(grep -cE "^CP: Replacing" "$debug_output")
    dce_count=$(grep -cE "^DCE: Removed" "$debug_output")
    cse_count=$(grep -cE "^CSE: Replacing" "$debug_output")

    echo "Optimization counts:" | tee -a "$OUTPUT_FILE"
    echo "  --> CP: $cp_count" | tee -a "$OUTPUT_FILE"
    echo "  --> DCE: $dce_count" | tee -a "$OUTPUT_FILE"
    echo "  --> CSE: $cse_count" | tee -a "$OUTPUT_FILE"

    total_cp=$((total_cp + cp_count))
    total_dce=$((total_dce + dce_count))
    total_cse=$((total_cse + cse_count))
}

# Loop through all files in 'input' directory
for file in "$BASE_DIR/$INPUT_DIR"/*; do
    if [[ -f "$file" ]]; then
        filename=$(basename -- "$file")
        expected_output="$BASE_DIR/$OUTPUT_DIR/$filename.out"
        assembly_file="$file.s"
        executable_file="$file.out"
        actual_output="$file.result"
        debug_output="$file.debug"

        rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"

        echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
        echo "Running: ./run.sh -t assembly \"$file\" $OPT_FLAGS --debug" | tee -a "$OUTPUT_FILE"
        echo "----------------------------------------" | tee -a "$OUTPUT_FILE"

        ./run.sh -t assembly "$file" $OPT_FLAGS --debug -o "$assembly_file" > "$debug_output"
        exit_code=$?

        if [[ $exit_code -ne 0 ]]; then
            echo "Compilation failed for $file" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
            rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"
            continue
        fi

        count_optimizations "$debug_output"

        gcc -O0 -no-pie "$assembly_file" -o "$executable_file"
        if [[ $? -ne 0 ]]; then
            echo "Assembly linking failed for $file" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
            rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"
            continue
        fi

        "$executable_file" > "$actual_output"
        exit_code=$?
        if [[ $exit_code -ne 0 ]]; then
            echo "Execution for $file returned with an error" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
            rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"
            continue
        fi

        if compare_outputs "$actual_output" "$expected_output"; then
            echo "Test passed for $file" | tee -a "$OUTPUT_FILE"
            correct=$((correct + 1))
        else
            echo "Test failed for $file" | tee -a "$OUTPUT_FILE"
            echo "Differences:" | tee -a "$OUTPUT_FILE"
            diff "$actual_output" "$expected_output" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
        fi

        rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"
    fi
done

# Loop through all files in 'error' directory
for file in "$BASE_DIR/$ERROR_DIR"/*; do
    if [[ -f "$file" ]]; then
        filename=$(basename -- "$file")
        assembly_file="$file.s"
        executable_file="$file.out"
        actual_output="$file.result"
        debug_output="$file.debug"

        rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"

        echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
        echo "Running: ./run.sh -t assembly \"$file\" $OPT_FLAGS --debug" | tee -a "$OUTPUT_FILE"
        echo "----------------------------------------" | tee -a "$OUTPUT_FILE"

        ./run.sh -t assembly "$file" $OPT_FLAGS --debug -o "$assembly_file" > "$debug_output"
        exit_code=$?

        if [[ $exit_code -ne 0 ]]; then
            echo "Compilation failed for $file" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
            rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"
            continue
        fi

        count_optimizations "$debug_output"

        gcc -O0 -no-pie "$assembly_file" -o "$executable_file"
        if [[ $? -ne 0 ]]; then
            echo "Assembly linking failed for $file" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
            rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"
            continue
        fi

        "$executable_file"
        exit_code=$?
        if [[ $exit_code -ne 0 ]]; then
            echo "Correctly failed at execution for $file" | tee -a "$OUTPUT_FILE"
            correct=$((correct + 1))
        else
            echo "Incorrectly succeeded for $file" | tee -a "$OUTPUT_FILE"
            incorrect=$((incorrect + 1))
        fi

        rm -f "$assembly_file" "$executable_file" "$actual_output" "$debug_output"
    fi
done

# ==========================
# Print final summary
# ==========================
echo -e "\n========================================" | tee -a "$OUTPUT_FILE"
echo "Execution completed. Results saved in $OUTPUT_FILE." | tee -a "$OUTPUT_FILE"
echo "Total Passed: $correct" | tee -a "$OUTPUT_FILE"
echo "Total Failed: $incorrect" | tee -a "$OUTPUT_FILE"
echo "Total Optimizations:" | tee -a "$OUTPUT_FILE"
echo "  --> Total CP: $total_cp" | tee -a "$OUTPUT_FILE"
echo "  --> Total DCE: $total_dce" | tee -a "$OUTPUT_FILE"
echo "  --> Total CSE: $total_cse" | tee -a "$OUTPUT_FILE"

if [[ $incorrect -gt 0 ]]; then
    exit 1
else
    exit 0
fi
