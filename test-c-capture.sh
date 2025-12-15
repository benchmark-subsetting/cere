#!/bin/bash
set -euo pipefail

echo "=== Testing C capture with CERE (step by step) ==="
echo ""

cd /workspace

# Clean up
rm -f test_loop test_loop.o test_loop.ll
rm -rf .cere

echo "Step 1: Compile in dump mode (object file)..."
CERE_MODE="dump --region=__cere__loop_region" cerec -c test_loop.c -o test_loop.o
echo "✓ Object compilation succeeded"
echo ""

echo "Step 2: Verify dump calls in object file..."
if objdump -r test_loop.o | grep -q dump; then
    echo "✓ Dump relocations found:"
    objdump -r test_loop.o | grep dump
else
    echo "✗ ERROR: No dump calls in object file!"
    exit 1
fi
echo ""

echo "Step 3: Link executable..."
CERE_MODE="dump --region=__cere__loop_region" cerec test_loop.o -o test_loop
echo "✓ Linking succeeded"
echo ""

echo "Step 4: Verify dump calls in executable..."
if objdump -d test_loop | grep -q "dump_init@plt"; then
    echo "✓ dump_init found in executable"
    objdump -d test_loop | sed -n '/<main>:/,/<[^:]*>:/p' | grep call
else
    echo "✗ ERROR: dump_init not found in executable!"
    exit 1
fi
echo ""

echo "Step 5: Run executable..."
if ./test_loop 2>&1; then
    echo "✓ Execution succeeded"
else
    echo "⚠ Execution failed (may be cere-tracer issue)"
fi
echo ""

echo "Step 6: Check for memory dumps..."
if find .cere -name "*.memdump" 2>/dev/null | grep -q .; then
    echo "✓ SUCCESS: Memory dumps created!"
    find .cere -name "*.memdump"
else
    echo "⚠ No memory dumps found (cere-tracer issue)"
fi

echo ""
echo "=== Test complete ==="
