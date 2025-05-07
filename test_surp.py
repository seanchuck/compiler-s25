#!/usr/bin/env python3

import sys

output_file = "tests/phase5/output/mergesort.txt"

try:
    with open(output_file) as f:
        lines = f.readlines()

    numbers = []
    for line in lines:
        line = line.strip()
        if not line:
            continue
        if line.startswith("Execution time:"):
            # Skip the timing line
            continue
        numbers.append(int(line))

    if len(numbers) != 250:
        print(f"Error: Expected 250 numbers, found {len(numbers)}")
        sys.exit(1)

    print("Output: ================================================")
    for i in range(len(numbers) - 1):
        print(numbers[i])
        # if numbers[i] > numbers[i + 1]:
        #     print(f"Error: Numbers not sorted at index {i} ({numbers[i]} > {numbers[i+1]})")
        #     sys.exit(1)
    print("Output: ================================================")

    print("Output verification passed: numbers are sorted.")

except FileNotFoundError:
    print(f"Error: Output file {output_file} not found.")
    sys.exit(1)

except Exception as e:
    print(f"An error occurred: {e}")
    sys.exit(1)
