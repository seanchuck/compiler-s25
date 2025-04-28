#!/bin/bash
# run with "hyperfine --runs 30 ./time.sh" to get a benchmark time for 30 runs of all tests

# Set the executable directory
EXECUTABLE_DIR="local/exec"

# Run all executables
for file in "$EXECUTABLE_DIR"/*.out; do
    "$file"
done
