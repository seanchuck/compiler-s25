#!/usr/bin/env bash
set -uo pipefail
# Don’t use `-e` so failures don’t abort the whole run
shopt -s nullglob

ROOT="public-tests/phase2-semantics"
DIRS=(
  "$ROOT/public/legal"
  "$ROOT/public/illegal"
  "$ROOT/private/legal"
  "$ROOT/private/illegal"
)

total=0
passed=0
failed=0

echo "=== Running Phase 2 Semantics Tests ==="

for dir in "${DIRS[@]}"; do
  echo
  echo "=== Directory: $dir ==="
  if [[ ! -d $dir ]]; then
    echo "  → Skipping, not found"
    continue
  fi

  # Expand every file in the directory
  for file in "$dir"/*; do
    # If nullglob, an empty dir yields no iterations—good
    [[ -f $file ]] || continue

    (( total++ ))
    echo "----------------------------------------"
    echo "Test #$total: $file"
    echo "Command: ./run.sh \"$file\" -t inter"
    
    # Run it, capture exit code without exiting script
    ./run.sh "$file" -t inter
    code=$?

    # Determine expectation from dir name
    if [[ $dir == */legal ]]; then
      if [[ $code -eq 0 ]]; then
        echo "✔ PASS (exit 0 as expected)"
        (( passed++ ))
      else
        echo "✘ FAIL (unexpected exit $code)"
        (( failed++ ))
      fi
    else  # illegal
      if [[ $code -ne 0 ]]; then
        echo "✔ PASS (errored as expected)"
        (( passed++ ))
      else
        echo "✘ FAIL (unexpected success)"
        (( failed++ ))
      fi
    fi
  done
done

echo
echo "========================================"
echo "Total tests: $total"
echo "Passed:      $passed"
echo "Failed:      $failed"

# Exit nonzero if any failures
(( failed > 0 )) && exit 1 || exit 0
