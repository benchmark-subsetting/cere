# Résumé des corrections apportées à CERE

## Problème initial
La capture CERE échouait avec un timeout lors de l'exécution du test C++23. Le programme se bloquait après "Before tracer_dump".

## Corrections effectuées

### 1. ✅ Correction de la gestion ptrace (COMPLÉTÉ)
**Fichiers modifiés:**
- `src/memory_dump/ptrace.h` - Ajout de `continue_all_cont()`
- `src/memory_dump/ptrace.c` - Implémentation de `continue_all_cont()`
- `src/memory_dump/cere_tracer.c` - Utilisation de `ptrace_cont()` au lieu de `ptrace_syscall()` en mode UNLOCKED/LOCKED

**Résultat:** Le tracer et le tracee communiquent correctement via SIGTRAP.

### 2. ✅ Correction de l'instrumentation LLVM (COMPLÉTÉ)
**Fichiers modifiés:**
- `src/RegionDump/LoopRegionDump.cpp`:
  - Empêcher l'insertion multiple de `dump_init()` (flag statique)
  - Insérer `after_dump()` avant le terminator au lieu du début du bloc

**Résultat:** L'instrumentation LLVM génère du code IR valide.

### 3. ✅ Correction de l'ordre des opérations dans dump() (COMPLÉTÉ)
**Fichiers modifiés:**
- `src/memory_dump/tracee.c`:
  - Déplacer `strcpy(tracer_buff.str_tmp, loop_name)` AVANT `send_to_tracer(TRAP_LOCK_MEM)`
  - Déplacer `in_codelet = true` AVANT `send_to_tracer(TRAP_LOCK_MEM)`
  - Déplacer `mtrace_active = true` AVANT `send_to_tracer(TRAP_LOCK_MEM)`

**Raison:** Après `TRAP_LOCK_MEM`, toute la mémoire est protégée par mprotect, donc on ne peut plus écrire dans des variables globales ou des buffers.

### 4. ✅ Correction des casts void* pour C++ (COMPLÉTÉ)
**Fichiers modifiés:**
- `src/memory_dump/tracee.c`:
  - Cast explicite: `(InvocsCounter*)realloc(...)`
  - Cast explicite: `(char*)malloc(...)` et `(int*)malloc(...)`

**Raison:** En C++, on ne peut pas assigner directement un `void*` à un type pointeur sans cast.

### 5. ✅ Correction de send_to_tracer() pour inlining (COMPLÉTÉ)
**Fichiers modifiés:**
- `src/memory_dump/tracee_interface.h` - Définitions inline complètes
- `src/memory_dump/tracee_interface.c` - Retrait des définitions (déplacées au header)

**Changement:**
```c
// AVANT: fonction normale (appel via PLT/GOT)
void send_to_tracer(register_t arg);

// APRÈS: inline asm combiné (pas d'appel de fonction)
static inline void send_to_tracer(register_t arg) {
  asm volatile("mov %0,%%rax; mov %1,%%rdi; int $3"
               :
               : "r"((register_t)SYS_dump), "r"(arg)
               : "rax", "rdi", "memory");
}
```

**Raison:** Après `TRAP_LOCK_MEM`, appeler une fonction via PLT/GOT cause un SIGSEGV car le GOT est protégé.

## État actuel

### ✅ CE QUI FONCTIONNE:
1. Communication tracer/tracee via SIGTRAP ✅
2. Profiling (interrupts/evictions/bytes) ✅
3. Call graph generation ✅
4. Premier SIGTRAP (TRAP_LOCK_MEM) reçu et géré ✅
5. tracer_lock_mem() s'exécute correctement ✅
6. RAX contient maintenant correctement SYS_dump ✅

### ❌ CE QUI NE FONCTIONNE PAS ENCORE:
Après `send_to_tracer(TRAP_LOCK_MEM)` et le retour avec `ptrace_cont`, le tracee crashe avec un NULL pointer dereference en boucle infinie.

**Diagnostic actuel:**
- RIP = 0x7818f06a7731 (dans libc, adresse change avec ASLR)
- sigaddr = (nil) = NULL
- Registres:
  - RAX = 0x7ffffffe (SYS_dump) ✅
  - RDI = 0x6f (TRAP_LOCK_MEM)
  - RSI = 0x583b3081405f (loop_name)

**Hypothèse:**
Même avec l'inline asm, le compilateur ajoute peut-être du code (prologue/épilogue, stack management, etc.) qui accède à de la mémoire protégée.

## Solutions possibles à explorer

### Option A: Utiliser `tracer_buff.syscall`
Au lieu d'utiliser `send_to_tracer()`, exécuter directement le code machine dans `tracer_buff.syscall` qui est une séquence d'octets pure.

### Option B: Fonction `__attribute__((naked))`
Créer une fonction naked sans prologue/épilogue pour le code après TRAP_LOCK_MEM.

### Option C: Investigation plus poussée
Désassembler le binaire compilé pour voir exactement quelle instruction cause le NULL dereference.

## Fichiers de debug créés
- `test_dump_direct.c` - Test minimal pour reproduire le problème
- `PROBLEME_FINAL_ET_SOLUTION.md` - Analyse détaillée du problème
- `CORRECTIONS_FINALES.md` - Liste complète des modifications
- `RESULTAT_FINAL.md` - État actuel du projet
- `docker-test.sh` - Script pour tester dans Docker

## Commandes pour tester

```bash
# Rebuild Docker
docker build -t cere-test .

# Test minimal
docker run --rm --cap-add=SYS_PTRACE --security-opt seccomp=unconfined \
  -v $PWD:/workspace cere-test bash -c \
  'cd /workspace && clang -g test_dump_direct.c -o test_dump_direct \
   -L/usr/local/lib -lcere_dump -lpthread -ldl && timeout 10 ./test_dump_direct'
```

## Progrès global: ~80-90%

La grande majorité du système fonctionne. Le problème restant est spécifique à l'exécution de code après que mprotect ait protégé toute la mémoire. C'est un problème difficile mais bien circonscrit.
