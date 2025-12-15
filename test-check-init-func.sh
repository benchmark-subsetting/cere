#!/bin/bash
set -euo pipefail

echo "=== Checking which init function is called ==="
echo ""

cd /workspace

# Clean and compile
rm -f test_loop test_loop.o
CERE_MODE="dump --region=__cere__loop_region" cerec -c test_loop.c -o test_loop.o
CERE_MODE="dump --region=__cere__loop_region" cerec test_loop.o -o test_loop

echo "Checking for dump_init calls..."
objdump -d test_loop | grep -E "call.*dump_init|call.*multi_dump_init" || echo "No init calls found"

echo ""
echo "Checking relocations in object file..."
objdump -r test_loop.o | grep -E "dump_init|multi_dump_init" || echo "No init relocations"

echo ""
echo "Checking symbols..."
nm test_loop | grep -E "dump_init|multi_dump_init" || echo "No init symbols"

echo ""
echo "=== Done ==="
