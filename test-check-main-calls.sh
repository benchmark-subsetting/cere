#!/bin/bash
set -euo pipefail

echo "=== Checking what main() calls ==="
echo ""

cd /workspace

echo "Disassembly of main():"
objdump -d test_loop | sed -n '/<main>:/,/^$/p'

echo ""
echo "=== Done ==="
