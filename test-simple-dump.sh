#!/bin/bash
set -euo pipefail

echo "=== Testing full dump cycle ==="
echo ""

cd /workspace

# Clean up
rm -f test_loop test_loop.o
rm -rf .cere

echo "Step 1: Compile with CERE in dump mode..."
CERE_MODE="dump --region=__cere__loop_region" cerec -c test_loop.c -o test_loop.o
CERE_MODE="dump --region=__cere__loop_region" cerec test_loop.o -o test_loop

echo "Step 2: Check dump_init calls in executable..."
objdump -d test_loop | grep "call.*dump_init" | wc -l

echo ""
echo "Step 3: Run with timeout..."
timeout 5 ./test_loop 2>&1 || {
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 124 ]; then
    echo ""
    echo "✗ BLOCKED after 5 seconds"
    echo "Checking processes..."
    ps aux | grep -E "(test_loop|cere-tracer)" | grep -v grep || true
  else
    echo "Exited with code $EXIT_CODE"
  fi
}

echo ""
echo "=== Done ==="
