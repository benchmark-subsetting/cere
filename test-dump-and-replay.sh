#!/bin/bash
set -e

echo "======================================"
echo "Test complet: DUMP et REPLAY"
echo "======================================"
echo ""

# Nettoyage
echo "Nettoyage des fichiers précédents..."
rm -rf .cere test_cere_loop test_cere_loop.ll lel_bin 2>/dev/null || true
echo ""

echo "=== PHASE 1: DUMP ==="
echo ""

echo "Step 1: Génération du LLVM IR"
clang -S -emit-llvm test_cere_loop.c -o test_cere_loop.ll
echo "  ✓ LLVM IR généré"
echo ""

echo "Step 2: Instrumentation avec region-dump"
/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/libcere_regiondump.so \
  -region-dump --region=__cere__loop_region \
  test_cere_loop.ll -o test_cere_loop.ll
echo "  ✓ Instrumentation appliquée"
echo ""

echo "Step 3: Vérification de l'instrumentation"
DUMP_INIT_COUNT=$(grep -c 'call.*dump_init' test_cere_loop.ll || echo 0)
DUMP_COUNT=$(grep -c 'call.*@dump' test_cere_loop.ll || echo 0)
AFTER_DUMP_COUNT=$(grep -c 'call.*after_dump' test_cere_loop.ll || echo 0)
echo "  - dump_init: $DUMP_INIT_COUNT appel(s)"
echo "  - dump: $DUMP_COUNT appel(s)"
echo "  - after_dump: $AFTER_DUMP_COUNT appel(s)"

if [ "$DUMP_COUNT" -eq 0 ]; then
  echo "  ❌ ERREUR: Pas d'appel à dump trouvé!"
  exit 1
fi
echo ""

echo "Step 4: Compilation de l'exécutable"
clang test_cere_loop.ll -o test_cere_loop \
  -L/usr/local/lib -lcere_dump -lpthread -ldl
echo "  ✓ Exécutable créé"
echo ""

echo "Step 5: Exécution pour capturer (timeout 10s)"
timeout 10 ./test_cere_loop 2>&1 || {
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 124 ]; then
    echo ""
    echo "❌ ERREUR: Programme bloqué (timeout)"
    exit 1
  fi
}
echo ""

echo "Step 6: Vérification des dumps créés"
if [ ! -d ".cere/dumps/__cere__loop_region" ]; then
  echo "  ❌ ERREUR: Répertoire .cere/dumps/__cere__loop_region non créé"
  exit 1
fi

if [ ! -d ".cere/dumps/__cere__loop_region/1" ]; then
  echo "  ❌ ERREUR: Dump invocation 1 non créé"
  exit 1
fi

echo "  ✓ Dumps créés avec succès!"
echo ""
echo "Contenu du dump:"
ls -lh .cere/dumps/__cere__loop_region/1/ 2>/dev/null || echo "  (vide)"
echo ""

echo "=== PHASE 2: REPLAY ==="
echo ""

echo "Step 7: Génération du code replay avec region-replay"
clang -S -emit-llvm test_cere_loop.c -o test_cere_loop_replay.ll

/usr/lib/llvm-16/bin/opt -enable-new-pm=0 -S \
  -load /usr/local/lib/libcere_regionreplay.so \
  -region-replay --region=__cere__loop_region --invocation=1 \
  test_cere_loop_replay.ll -o test_cere_loop_replay.ll
echo "  ✓ Code replay généré"
echo ""

echo "Step 8: Compilation de l'exécutable replay"
clang test_cere_loop_replay.ll -o test_cere_loop_replay \
  -L/usr/local/lib -lcere_load -lpthread -ldl
echo "  ✓ Exécutable replay créé"
echo ""

echo "Step 9: Exécution du replay"
./test_cere_loop_replay 2>&1
REPLAY_EXIT=$?
echo ""

if [ $REPLAY_EXIT -eq 0 ]; then
  echo "✅ SUCCÈS: Le dump ET le replay ont fonctionné!"
else
  echo "⚠️  Le dump a réussi mais le replay a échoué (code: $REPLAY_EXIT)"
fi

echo ""
echo "======================================"
echo "Test terminé"
echo "======================================"
