#!/bin/bash
set -euo pipefail

echo "=== Checking LLVM IR for injected calls ==="
echo ""

cd /workspace

rm -f test_loop.ll test_loop.o

echo "Generating LLVM IR in dump mode..."
CERE_MODE="dump --region=__cere__loop_region" cerec -S -emit-llvm test_loop.c -o test_loop.ll

echo ""
echo "=== Contents of main function ==="
sed -n '/define.*@main/,/^}/p' test_loop.ll | head -30

echo ""
echo "=== Contents of __cere__loop_region function ==="
sed -n '/define.*@__cere__loop_region/,/^}/p' test_loop.ll | head -40

echo ""
echo "=== Searching for dump/after_dump calls ==="
grep -n "call.*dump" test_loop.ll || echo "No dump calls found"

echo ""
echo "=== Done ==="
