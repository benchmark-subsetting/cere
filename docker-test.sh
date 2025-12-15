#!/bin/bash
set -e

echo "Lancement du test dans Docker..."
echo ""

docker run --rm \
  --cap-add=SYS_PTRACE \
  --security-opt seccomp=unconfined \
  -v "$PWD":/workspace \
  -w /workspace \
  cere-dev bash -c '
echo "=== Compilation des modifications récentes ==="
cd /workspace/src/memory_dump
make
make install
echo ""

echo "=== Vérification de l installation ==="
ldconfig /usr/local/lib
echo "Bibliothèques installées:"
ls -lh /usr/local/lib/libcere*.so 2>/dev/null | head -5
echo ""

cd /workspace

echo "=== Exécution du test dump et replay ==="
./test-dump-and-replay.sh
'
