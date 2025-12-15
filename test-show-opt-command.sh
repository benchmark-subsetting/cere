#!/bin/bash
set -x  # Show all commands

cd /workspace
rm -f test_loop.ll test_loop.o

# Compile to LLVM IR
clang -S -emit-llvm test_loop.c -o test_loop.ll

# Apply region-outliner pass
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/LLVMRegionOutliner.so \
  -region-outliner \
  test_loop.ll -o test_loop.ll

echo "=== After outliner pass ==="
grep -E "define.*__cere__" test_loop.ll | head -5

# Apply region-dump pass
echo ""
echo "Applying region-dump pass with --region=__cere__loop_region"
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/LLVMRegionDump.so \
  -region-dump \
  --region=__cere__loop_region \
  test_loop.ll -o test_loop.ll 2>&1 | head -20

echo ""
echo "=== After dump pass ==="
grep -n "call.*dump" test_loop.ll || echo "No dump calls found!"

echo ""
echo "=== Done ==="
