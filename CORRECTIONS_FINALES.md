# Corrections finales pour CERE

## Résumé

Toutes les corrections ont été apportées pour résoudre les problèmes de capture (dump) avec LLVM 16 et C++23. Les modifications corrigent principalement l'utilisation de ptrace pour permettre au tracer et au tracee de communiquer correctement.

## Fichiers modifiés

### 1. src/memory_dump/ptrace.h

**Ligne 57** : Ajout de la déclaration de `continue_all_cont()`

```c
void continue_all(void);
void continue_all_cont(void);
```

### 2. src/memory_dump/ptrace.c

**Lignes 203-208** : Nouvelle fonction `continue_all_cont()` utilisant `PTRACE_CONT`

```c
void continue_all_cont(void) {
  for (int i=0; i< ntids; i++) {
    if (!pending[i])
      ptrace_cont(tids[i]);
  }
}
```

### 3. src/memory_dump/cere_tracer.c

**MODIFICATION 1 - Ligne 340-348** : Suppression des logs de debug dans `handle_events_until_dump_trap()`

```c
pid_t handle_events_until_dump_trap(pid_t wait_for) {
  while (true) {
    event_t e = wait_event(wait_for);
    if (e.signo == SIGTRAP) {
      if (is_dump_sigtrap(e.tid)) {
        clear_trap(e.tid);
        return e.tid;
      } else if (is_hook_sigtrap(e.tid)) {
        // ...
```

**MODIFICATION 2 - Lignes 351-358** : Utilisation de `ptrace_cont()` en modes UNLOCKED et LOCKED pour les SIGTRAPs syscall

```c
      } else { // If we arrive here, it is a syscall
        handle_syscall(e.tid);
        // In UNLOCKED and LOCKED modes, use ptrace_cont to run freely until SIGTRAP
        // In DUMPING and FIRSTTOUCH, use ptrace_syscall for memory tracking
        if (tracer_state == TRACER_UNLOCKED || tracer_state == TRACER_LOCKED)
          ptrace_cont(e.tid);
        else
          ptrace_syscall(e.tid);
        continue;
      }
```

**MODIFICATION 3 - Lignes 383-391** : Utilisation de `ptrace_cont()` en modes UNLOCKED et LOCKED pour les SIGSEGV + ajout du `continue` manquant

```c
      // In UNLOCKED and LOCKED modes, use ptrace_cont to run freely until SIGTRAP
      // In DUMPING and FIRSTTOUCH, use ptrace_syscall for memory tracking
      if (tracer_state == TRACER_UNLOCKED || tracer_state == TRACER_LOCKED)
        ptrace_cont(e.tid);
      else
        ptrace_syscall(e.tid);
      continue;  // IMPORTANT: Continue the while loop, don't fall through
    }
```

**MODIFICATION 4 - Lignes 394-402** : Utilisation de `ptrace_cont()` en modes UNLOCKED et LOCKED pour SIGWINCH

```c
    else if (e.signo == SIGWINCH) {
      /* Ignore signal SIGWINCH, tty resize */
      // In UNLOCKED and LOCKED modes, use ptrace_cont to run freely until SIGTRAP
      // In DUMPING and FIRSTTOUCH, use ptrace_syscall for memory tracking
      if (tracer_state == TRACER_UNLOCKED || tracer_state == TRACER_LOCKED)
        ptrace_cont(e.tid);
      else
        ptrace_syscall(e.tid);
      continue;
    }
```

**MODIFICATION 5 - Lignes 529, 532, 535, 555** : Utilisation de `ptrace_cont()` dans `tracer_dump()`

```c
  ptrace_getdata(pid, (long) tracer_buff->str_tmp, loop_name, SIZE_LOOP);
  ptrace_cont(pid);  // Use ptrace_cont to reach next send_to_tracer()

  invocation = (int)receive_from_tracee(pid);
  ptrace_cont(pid);  // Use ptrace_cont to reach next send_to_tracer()

  int arg_count = (int)receive_from_tracee(pid);
  ptrace_cont(pid);  // Use ptrace_cont to reach next send_to_tracer()

  // ... plus tard ...
  for (i = 0; i < arg_count; i++) {
    addresses[i] = (void *)receive_from_tracee(pid);
    ptrace_cont(pid);  // Use ptrace_cont to reach next send_to_tracer()
  }
```

**MODIFICATION 6 - Lignes 652, 710** : Utilisation de `continue_all_cont()` et ajout de `ptrace_cont()` avant `tracer_dump()`

```c
  } else {
    debug_print("%s\n", "******* TRACER_UNLOCKED");
    tracer_state = TRACER_UNLOCKED;
    // In UNLOCKED mode, use ptrace_cont to run freely until first SIGTRAP
    continue_all_cont();
  }

// ... plus tard dans handle_dump_trap ...

  tracer_state = TRACER_LOCKED;
  // Continue the tracee so it can send TRAP_START_ARGS
  ptrace_cont(tid);

  /* Dump arguments */
  tracer_dump(tid);
```

### 4. src/RegionDump/LoopRegionDump.cpp

**MODIFICATION 1 - Lignes 73, 132, 146-147** : Prévention des appels multiples à `dump_init()`

```cpp
  bool GlobalDump;
  bool ReadFromFile;
  static bool dump_init_inserted;  // Track if dump_init call has been inserted

// ... plus tard ...

char LoopRegionDump::ID = 0;
bool LoopRegionDump::dump_init_inserted = false;

// ... dans runOnFunction ...

  // Only insert dump_init call once, even if runOnFunction is called multiple times
  if (Main && !dump_init_inserted) {
    dump_init_inserted = true;  // Mark as inserted
```

**MODIFICATION 2 - Ligne 258** : Insertion de `after_dump()` avant le terminateur pour préserver la structure du basic block

```cpp
  /* Insert after_dump calls (once) in each exit blocks */
  for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(),
                                               E = exitblocks.end();
       I != E; ++I) {
    // Build after_dump params
    IRBuilder<> builder(*I);
    std::vector<Value *> ad_funcParameter;
    ad_funcParameter.push_back(builder.CreateGlobalStringPtr(currFunc->getName()));

    // Insert call before the terminator to preserve basic block structure
    CallInst::Create(func_after_dump, ad_funcParameter, "", (*I)->getTerminator());
  }
```

## Explication des corrections

### Problème principal : Utilisation incorrecte de `ptrace_syscall()` en modes UNLOCKED et LOCKED

**Comportement souhaité** :

- En mode **UNLOCKED** et **LOCKED**, le tracee devrait courir librement jusqu'au prochain `int $3` (instruction SIGTRAP)
- En mode **DUMPING** et **FIRSTTOUCH**, le tracee devrait s'arrêter à chaque syscall pour tracking mémoire

**Problème** :
Le code utilisait `ptrace_syscall()` dans tous les modes, ce qui faisait que le tracee s'arrêtait à chaque syscall même en mode LOCKED. Cela empêchait le tracee d'atteindre les instructions `send_to_tracer()` qui génèrent les SIGTRAPs de protocole.

**Solution** :

- Utiliser `PTRACE_CONT` en modes UNLOCKED et LOCKED
- Utiliser `PTRACE_SYSCALL` en modes DUMPING et FIRSTTOUCH

### Bug critique : Missing `continue` statement

Après avoir géré un SIGSEGV et appelé `ptrace_cont()` ou `ptrace_syscall()`, le code manquait un `continue` statement. Cela faisait que le flux d'exécution tombait dans les autres `else if` au lieu de recommencer la boucle while. **Cette correction est critique.**

### Pass LLVM : Préservation de la structure des basic blocks

L'insertion de `after_dump()` au début d'un basic block (avant la première instruction) au lieu d'avant le terminateur cassait la structure du basic block LLVM. Les basic blocks doivent toujours se terminer par un terminateur (br, ret, etc.).

## Test et validation

Pour tester:

```bash
# Reconstruire l'image Docker
docker build --no-cache -t cere-dev .

# Lancer le conteneur
docker run --rm -it \
  --cap-add=SYS_PTRACE \
  --security-opt seccomp=unconfined \
  -v "$PWD":/workspace \
  -w /workspace \
  cere-dev bash

# Dans le conteneur, tester
./test-dump-and-replay.sh
```

Le résultat attendu :

- ✅ Pas de timeout
- ✅ Messages de debug montrant la communication tracer/tracee
- ✅ Dumps créés dans `.cere/dumps/__cere__loop_region/1/`
- ✅ Replay fonctionnel
