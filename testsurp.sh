#!/usr/bin/env bash
set -euo pipefail

# 1) Build your Decaf program (if necessary)
echo "Building surp executable"
./run.sh -t assembly -O all tests/phase5/surp.dcf -o test.s
gcc -no-pie test.s -L. -lruntime -o test

# 2) Run the Decaf‐generated binary
echo "Running merge‑sort..."
./test

# 3) Verify that the program produced the correct output
echo "Checking output..."
python3 test_surp.py

echo "Success: program ran without errors and output is correct."
