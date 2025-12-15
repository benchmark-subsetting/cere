#!/bin/bash
set -euo pipefail

echo "=== Testing dump_init() minimal ==="
echo ""

cd /workspace

# Compile without CERE (just link with dump library)
echo "Compiling test program..."
gcc test_dump_init_only.c -o test_dump_init_only -L/usr/local/lib -lcere_dump -lpthread -ldl

echo "Testing if cere-tracer is in PATH..."
which cere-tracer || echo "WARNING: cere-tracer not in PATH"
echo ""

echo "Running test program with strace to see what happens..."
timeout 5 strace -f -e trace=fork,execve,ptrace,kill,wait4 ./test_dump_init_only 2>&1 | head -100 || {
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 124 ]; then
    echo "TIMEOUT: Program blocked for 5+ seconds"
  else
    echo "Program exited with code $EXIT_CODE"
  fi
}

echo ""
echo "=== Test complete ==="
