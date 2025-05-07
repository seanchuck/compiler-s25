#!/usr/bin/env bash
set -euo pipefail

# 0) Compile the runtime
echo "Compiling runtime.c"
gcc -c runtime.c -o runtime.o

# 1) Build your Decaf program into assembly
echo "Building surp executable"
./run.sh -t assembly -O all tests/phase5/surp.dcf -o test.s

# 2) Link the assembly with runtime.o into final binary
echo "Linking..."
gcc -no-pie test.s runtime.o -o test

# 3) Run the Decaf‐generated binary
echo "Running merge-sort..."
./test

# 4) Verify that the program produced the correct output
echo "Checking output..."
python3 test_surp.py

echo "Success: program ran without errors and output is correct."
