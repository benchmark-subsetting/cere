#!/bin/bash
set -e

echo "=== Test simple capture ==="
rm -f test_loop test_loop.ll test_loop.o
rm -rf .cere/dumps 2>/dev/null || true

# Step 1: Compile to LLVM IR
echo "Step 1: Compile to LLVM IR"
clang -S -emit-llvm test_loop.c -o test_loop.ll

# Step 2: Apply region-outliner pass
echo "Step 2: Apply region-outliner pass"
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/LLVMRegionOutliner.so \
  -region-outliner \
  test_loop.ll -o test_loop.ll

# Step 3: Apply region-dump pass
echo "Step 3: Apply region-dump pass"
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/LLVMRegionDump.so \
  -region-dump \
  --region=__cere__loop_region \
  test_loop.ll -o test_loop.ll

# Step 4: Compile to executable
echo "Step 4: Compile to executable"
clang test_loop.ll -o test_loop -L/usr/local/lib -lcere_dump -lpthread -ldl

# Step 5: Run with timeout
echo "Step 5: Run executable..."
timeout 10 ./test_loop 2>&1 || echo "Exit code: $?"

echo "=== Done ==="
