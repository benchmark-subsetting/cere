# Instructions de reconstruction et de test

## Corrections apportées

Toutes les corrections pour résoudre le problème de capture ont été appliquées :

1. **LoopRegionDump.cpp** : Empêche les appels multiples à `dump_init()`
2. **ptrace.c/h** : Ajout de `continue_all_cont()` pour utiliser `PTRACE_CONT`
3. **cere_tracer.c** : Utilisation de `ptrace_cont()` en mode UNLOCKED et dans `tracer_dump()`
4. **cere_tracer.c** : Ajout de `ptrace_cont(tid)` avant `tracer_dump()` pour permettre au tracee d'envoyer `TRAP_START_ARGS`

## Étapes pour tester

### 1. Reconstruire l'image Docker

```bash
docker build --no-cache -t cere-dev .
```

Cette étape prend quelques minutes car elle :

- Installe toutes les dépendances (LLVM 16, Python, etc.)
- Compile CERE avec toutes les corrections
- Installe les bibliothèques dans `/usr/local/lib`

### 2. Lancer le conteneur Docker

```bash
docker run --rm -it \
  --cap-add=SYS_PTRACE \
  --security-opt seccomp=unconfined \
  -v "$PWD":/workspace \
  -w /workspace \
  cere-dev bash
```

Les options importantes :

- `--cap-add=SYS_PTRACE` : Autorise ptrace pour le tracer
- `--security-opt seccomp=unconfined` : Désactive les restrictions seccomp
- `-v "$PWD":/workspace` : Monte le répertoire actuel dans le conteneur

### 3. Exécuter le test dans Docker

```bash
./test-final-capture.sh
```

Ce script :

- Compile `test_loop.c` vers LLVM IR
- Applique les passes region-outliner et region-dump
- Compile l'exécutable instrumenté
- Exécute le programme avec un timeout de 10s
- Vérifie que les dumps sont créés dans `.cere/dumps/`

## Résultat attendu

Si tout fonctionne correctement, vous devriez voir :

```
======================================
Test final de la capture C avec CERE
======================================

Step 1: Compilation vers LLVM IR
  ✓ test_loop.ll créé

Step 2: Application du pass region-outliner
  ✓ Régions extraites

Step 3: Application du pass region-dump
  ✓ Instrumentation dump ajoutée

Step 4: Vérification de l'instrumentation
  Nombre d'appels à dump_init: 1
  Nombre d'appels à dump: 1
  Nombre d'appels à after_dump: 1

Step 5: Compilation vers exécutable
  ✓ Exécutable créé

Step 6: Exécution du programme instrumenté
  (timeout de 10 secondes)

DUMP( __cere__loop_region 1 count = 2)
Result: 55

Step 7: Vérification des dumps créés
  ✓ Répertoire de dump créé: .cere/dumps/__cere__loop_region/1

Contenu du dump:
[fichiers de dump...]

✅ SUCCÈS: La capture a fonctionné!

======================================
Test terminé avec succès!
======================================
```

## En cas d'échec

Si le test échoue, vérifier :

1. **Timeout** : Le programme se bloque → Problème de ptrace
2. **No dump directory** : Le dump n'est pas créé → Problème de tracer
3. **Multiple dump_init** : Plus d'un appel à dump_init → Problème de passe LLVM

Consulter les logs du tracer pour identifier le point de blocage.

## Nettoyage

Pour nettoyer les fichiers de test :

```bash
rm -f test_loop test_loop.ll test_loop.o test_loop.s lel_bin
rm -rf .cere/
```
