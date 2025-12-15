#!/bin/bash
set -euo pipefail

echo "=== Testing dump_init() simple ==="
echo ""

cd /workspace

# Compile without CERE (just link with dump library)
echo "Compiling test program..."
gcc test_dump_init_only.c -o test_dump_init_only -L/usr/local/lib -lcere_dump -lpthread -ldl

echo ""
echo "Checking cere-tracer exists..."
if [ -f /usr/local/bin/cere-tracer ]; then
    echo "✓ cere-tracer exists at /usr/local/bin/cere-tracer"
    ls -la /usr/local/bin/cere-tracer
else
    echo "✗ ERROR: cere-tracer NOT found!"
    exit 1
fi

echo ""
echo "Running test program with 5 second timeout..."
timeout 5 ./test_dump_init_only 2>&1 || {
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 124 ]; then
    echo ""
    echo "✗ TIMEOUT: Program blocked!"
    echo "Checking what processes are running..."
    ps aux | grep -E "(test_dump|cere-tracer)" | grep -v grep || true
    pkill -9 test_dump_init_only 2>/dev/null || true
    pkill -9 cere-tracer 2>/dev/null || true
  else
    echo "Program exited with code $EXIT_CODE"
  fi
}

echo ""
echo "=== Test complete ==="
