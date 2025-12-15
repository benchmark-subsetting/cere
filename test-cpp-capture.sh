#!/bin/bash
set -euo pipefail

echo "=== Testing C++ capture with CERE ==="

# Step 1: Compile example to find mangled name
echo ""
echo "Step 1: Compiling example to find mangled names..."
cd examples/cpp23/build
cerec++ -c -g -std=gnu++20 ../../example.cpp -o example_test.o

# Find the mangled name
echo ""
echo "Step 2: Finding mangled name for __cere__compute_region..."
MANGLED_NAME=$(nm example_test.o | grep "__cere__compute_region" | awk '{print $3}')
echo "Found mangled name: $MANGLED_NAME"

# Step 3: Test compilation in dump mode with mangled name
echo ""
echo "Step 3: Testing dump mode compilation with mangled name..."
if [ -n "$MANGLED_NAME" ]; then
    echo "Trying: CERE_MODE=\"dump --region=$MANGLED_NAME\""
    rm -f example_test_dump.o
    CERE_MODE="dump --region=$MANGLED_NAME" cerec++ -c -g -std=gnu++20 ../../example.cpp -o example_test_dump.o

    # Check if dump calls are present
    echo ""
    echo "Step 4: Checking for dump calls..."
    if objdump -r example_test_dump.o | grep -q dump; then
        echo "✓ SUCCESS: dump calls found in object file!"
        objdump -r example_test_dump.o | grep dump
    else
        echo "✗ FAILED: No dump calls found"
    fi
else
    echo "✗ ERROR: Could not find mangled name"
    echo ""
    echo "All symbols in object file:"
    nm example_test.o | grep compute
fi

echo ""
echo "=== Test complete ==="
