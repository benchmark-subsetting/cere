#!/bin/bash
set -e

echo "======================================"
echo "Test final de la capture C avec CERE"
echo "======================================"
echo ""

# Nettoyage
rm -f test_loop test_loop.ll test_loop.o test_loop.s
rm -rf .cere/dumps 2>/dev/null || true
rm -f lel_bin 2>/dev/null || true

echo "Step 1: Compilation vers LLVM IR"
clang -S -emit-llvm test_loop.c -o test_loop.ll
echo "  ✓ test_loop.ll créé"
echo ""

echo "Step 2: Application du pass region-outliner"
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/libcere_regionoutliner.so \
  -region-outliner \
  test_loop.ll -o test_loop.ll
echo "  ✓ Régions extraites"
grep -c "define.*__cere__loop_region" test_loop.ll || echo "  WARNING: Pas de région trouvée!"
echo ""

echo "Step 3: Application du pass region-dump"
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/libcere_regiondump.so \
  -region-dump \
  --region=__cere__loop_region \
  test_loop.ll -o test_loop.ll
echo "  ✓ Instrumentation dump ajoutée"
echo ""

echo "Step 4: Vérification de l'instrumentation"
echo "  Nombre d'appels à dump_init: $(grep -c 'call.*dump_init' test_loop.ll || echo 0)"
echo "  Nombre d'appels à dump: $(grep -c 'call.*@dump' test_loop.ll || echo 0)"
echo "  Nombre d'appels à after_dump: $(grep -c 'call.*after_dump' test_loop.ll || echo 0)"
echo ""

echo "Step 5: Compilation vers exécutable"
clang test_loop.ll -o test_loop \
  -L/usr/local/lib -lcere_dump -lpthread -ldl
echo "  ✓ Exécutable créé"
echo ""

echo "Step 6: Exécution du programme instrumenté"
echo "  (timeout de 10 secondes)"
echo ""
timeout 10 ./test_loop 2>&1 || {
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 124 ]; then
    echo ""
    echo "❌ ERREUR: Programme bloqué (timeout)"
    exit 1
  else
    echo ""
    echo "Programme terminé avec code: $EXIT_CODE"
  fi
}
echo ""

echo "Step 7: Vérification des dumps créés"
if [ -d ".cere/dumps/__cere__loop_region/1" ]; then
  echo "  ✓ Répertoire de dump créé: .cere/dumps/__cere__loop_region/1"
  echo ""
  echo "Contenu du dump:"
  ls -lh .cere/dumps/__cere__loop_region/1/
  echo ""
  echo "✅ SUCCÈS: La capture a fonctionné!"
else
  echo "  ❌ ERREUR: Pas de dump créé dans .cere/dumps/"
  exit 1
fi

echo ""
echo "======================================"
echo "Test terminé avec succès!"
echo "======================================"
