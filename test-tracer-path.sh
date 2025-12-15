#!/bin/bash
echo "=== Checking cere-tracer availability ==="
echo ""

echo "Searching for cere-tracer in PATH..."
if which cere-tracer; then
    echo "✓ cere-tracer found in PATH"
else
    echo "✗ ERROR: cere-tracer NOT in PATH!"
fi
echo ""

echo "Searching for cere-tracer in /usr/local/bin..."
if [ -f /usr/local/bin/cere-tracer ]; then
    echo "✓ cere-tracer exists at /usr/local/bin/cere-tracer"
    ls -la /usr/local/bin/cere-tracer
else
    echo "✗ ERROR: cere-tracer NOT found at /usr/local/bin/cere-tracer"
fi
echo ""

echo "Trying to run cere-tracer with --help..."
cere-tracer --help 2>&1 || echo "cere-tracer failed or doesn't accept --help"
echo ""

echo "Checking if dump_init and dump libraries exist..."
ls -la /usr/local/lib/libcere_dump.* 2>&1 | head -5
echo ""

echo "=== Check complete ==="
