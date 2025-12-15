#!/bin/bash
set -euo pipefail

echo "=== Testing manual dump() calls with verbose output ==="
echo ""

cd /workspace

echo "Compiling verbose test..."
gcc test_loop_verbose.c -o test_loop_verbose \
  -L/usr/local/lib -lcere_dump -lpthread -ldl

echo ""
echo "Running with 5 second timeout..."
timeout 5 ./test_loop_verbose 2>&1 || {
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 124 ]; then
    echo ""
    echo "✗ TIMEOUT: Last output shows where it blocked"
  fi
}

echo ""
echo "=== Done ==="
