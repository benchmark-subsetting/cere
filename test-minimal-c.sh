#!/bin/bash
set -e

echo "=== Test minimal C ==="

# Créer un fichier de test C simple
cat > /tmp/test_minimal.c << 'EOF'
#include <stdio.h>

int __cere__compute_region(int a, int b) {
    return a + b;
}

int main() {
    int result = __cere__compute_region(5, 3);
    printf("Result: %d\n", result);
    return 0;
}
EOF

cd /tmp
rm -rf .cere test_minimal test_minimal.ll

# Compilation et instrumentation
echo "Step 1: LLVM IR"
clang -S -emit-llvm test_minimal.c -o test_minimal.ll

echo "Step 2: region-dump"
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/libcere_regiondump.so \
  -region-dump --region=__cere__compute_region \
  test_minimal.ll -o test_minimal.ll

echo "Step 3: Compile"
clang test_minimal.ll -o test_minimal \
  -L/usr/local/lib -lcere_dump -lpthread -ldl

echo "Step 4: Run"
timeout 5 ./test_minimal 2>&1 || echo "Exit code: $?"

echo ""
echo "Step 5: Check dumps"
if [ -d ".cere/dumps/__cere__compute_region/1" ]; then
  echo "✅ Dumps created!"
  ls -la .cere/dumps/__cere__compute_region/1/
else
  echo "❌ No dumps created"
fi
