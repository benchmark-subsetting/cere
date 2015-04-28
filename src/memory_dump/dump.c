#define _LARGEFILE64_SOURCE
#include <stdarg.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <stdbool.h>
#include <malloc.h>
#include <errno.h>
#include <sys/syscall.h>
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "dump.h"
#include "counters.h"
#include "err.h"

#define _DEBUG 1
#undef _DEBUG

struct dump_state state __attribute__((aligned(PAGESIZE)));
extern const char *__progname;

static char hex(char value) {
  const char conversion[] = "0123456789abcdef";
  assert(value >= 0 && value <= 15);
  return conversion[value];
}

static char *append_addr(char *buf, off64_t addr) {
  int digits = MAX_DIGITS;

  size_t i, j;

  while (digits--) {
    buf[i + digits] = hex(addr % 16);
    addr = addr / 16;
  }
  i += MAX_DIGITS;

  return buf + i;
}

static void debug(const char msg[]) { syscall(SYS_write, 2, msg, strlen(msg)); }

static void debug_addr(const char msg[], off64_t addr) {
  char buf[14];
  char *end = append_addr(buf, addr);
  *end = '\n';
  *(end + 1) = '\0';

  debug(msg);
  debug(buf);
}

bool is_mru(void *addr) {
  char *start_of_page = round_to_page(addr);
  bool present = false;
  for (int i = 0; i < state.log_size; i++) {
    if (state.pages_cache[i] == start_of_page) {
      present = true;
      break;
    }
  }
  return present;
}

static void mru_handler(int sig, siginfo_t *si, void *unused) {
  int esaved = errno;
  char *touched_addr = si->si_addr;
  char *start_of_page = round_to_page(touched_addr);

#ifdef _DEBUG
  debug_addr("MRU Detected access at ", (off64_t) touched_addr);
#endif

  bool present = is_mru(start_of_page);
  assert(present == false);

  /* Unprotect Page */
  int result = syscall(SYS_mprotect, (void *)start_of_page, PAGESIZE,
                       PROT_READ | PROT_WRITE | PROT_EXEC);
  if (result == -1) {
    assert(false);
  }
  /* Add page to page cache */
  /* we need to evict one of the pages, reprotect it ! */
  if (state.pages_cache[state.last_page] != 0) {

    /* add the evicted page to the trace */
    state.pages_trace[state.last_trace] = state.pages_cache[state.last_page];
    state.last_trace = (state.last_trace + 1) % TRACE_SIZE;

#ifdef _DEBUG
    debug_addr("MRU Reprotecting page ",
               (off64_t) state.pages_cache[state.last_page]);
#endif
    int result =
        syscall(SYS_mprotect, (void *)state.pages_cache[state.last_page],
                PAGESIZE, PROT_NONE);
    if (result == -1) {
      //assert(false);
    }
  }

  state.pages_cache[state.last_page] = start_of_page;
  state.last_page = (state.last_page + 1) % state.log_size;

#ifdef _DEBUG
  debug("     >>>> page cache\n");
  for (int i = 0; i < state.log_size; i++) {
    int c = (i + state.last_page) % state.log_size;
    debug_addr("     >>>> ", (off64_t) state.pages_cache[c]);
  }
#endif
  errno = esaved;
}

static void fill_dump_name(char *buf, int stack_pos, off64_t addr) {
  size_t i, j;
  for (i = 0; state.dump_path[stack_pos][i] != '\0'; i++)
    buf[i] = state.dump_path[stack_pos][i];
  buf[i++] = '/';

  buf = append_addr(buf + i, addr);

  const char suffix[] = ".memdump";
  i = 0;
  for (j = 0; suffix[j] != '\0'; j++)
    buf[i++] = suffix[j];

  buf[i++] = '\0';
}

/* dump_region: dumps to a file "<start>.memdump"
 * the memory segment starting at address <start>
 * and ending at address <end>
 */
static bool write_page(char path[], off64_t start) {
  int out = syscall(SYS_open, path, O_WRONLY | O_CREAT | O_EXCL, S_IRWXU);

  if (out <= 0) {
    /* region already dumped */
    if (errno == EEXIST)
      return false;
    else {
      debug("could not open:");
      debug(path);
      debug("\n");
      exit(EXIT_FAILURE);
    }
  }

  int wr = syscall(SYS_write, out, (char *)start, PAGESIZE);
  if (wr < 0) {
    syscall(SYS_unlink, path);
    syscall(SYS_close, out);
    return false;
  }
  assert(wr == PAGESIZE);

  syscall(SYS_close, out);
  return true;
}

static bool dump_page(off64_t start) {
  /* Dump Page */
  char current_path[MAX_PATH];
  fill_dump_name(current_path, state.stack_pos, start);
  bool result = write_page(current_path, start);
  if (result) {
    /* Link in parent's dumps */
    for (int i = 0; i < state.stack_pos; i++) {
      char parent_path[MAX_PATH];
      fill_dump_name(parent_path, i, start);
      syscall(SYS_link, current_path, parent_path);
    }
  }
  return result;
}

static void dump_handler(int sig, siginfo_t *si, void *unused) {

  int esaved = errno;
  bool old_status = state.mtrace_active;
  state.mtrace_active = false;
  char *touched_addr = si->si_addr;
  char *start_of_page = round_to_page(touched_addr);

#ifdef _DEBUG
  debug_addr("DUMP Detected access at: ", (off64_t) touched_addr);
#endif

  mru_handler(sig, si, unused);

  /* Dump page */
  dump_page((off64_t) start_of_page);

  /* Do the mru logging */
  state.mtrace_active = old_status;
  errno = esaved;
}

static void ignore_handler(int sig, siginfo_t *si, void *unused) {
  char *touched_addr = si->si_addr;
  char *start_of_page = round_to_page(touched_addr);

  /* Unprotect Page */
  int result = syscall(SYS_mprotect, (void *)start_of_page, PAGESIZE,
                       PROT_READ | PROT_WRITE | PROT_EXEC);
  assert(result != -1);

  assert(state.last_ignored < MAX_IGNORE);
  state.pages_ignored[state.last_ignored++] = start_of_page;

#ifdef _DEBUG
  debug_addr("IGN Detected access at: ", (off64_t) touched_addr);
#endif
}

static void flush_hot_pages_trace_to_disk(void) {
  char path[MAX_PATH];
  char buf[14];

  snprintf(path, sizeof(path), "%s/%s", state.dump_path[state.stack_pos],
           state.pagelog_suffix);
  int out = syscall(SYS_open, path, O_WRONLY | O_CREAT | O_EXCL, S_IRWXU);

  assert(out > 0);

  /* Dump the trace */
  for (int i = 0; i < TRACE_SIZE; i++) {
    int c = (i + state.last_trace) % TRACE_SIZE;
    if (state.pages_trace[c] != 0) {
      char *end = append_addr(buf, (off64_t) state.pages_trace[c]);
      *end = '\n';
      int wr = syscall(SYS_write, out, buf, end - buf + 1);
      assert(wr > 0);
    }
  }

  /* Dump the log */
  for (int i = 0; i < state.log_size; i++) {
    int c = (i + state.last_page) % state.log_size;
    if (state.pages_cache[c] != 0) {
      char *end = append_addr(buf, (off64_t) state.pages_cache[c]);
      *end = '\n';
      int wr = syscall(SYS_write, out, buf, end - buf + 1);
      assert(wr > 0);
    }
  }
  syscall(SYS_close, out);
}

static void dump_unprotected_pages(void) {
  /* Dump the unprotected pages before entering the codelet region. */
  for (int i = 0; i < state.log_size; i++) {
    int c = (i + state.last_page) % state.log_size;
    if (state.pages_cache[c] != 0) {
      bool result = dump_page((off64_t) state.pages_cache[c]);
    }
  }
}

static void page_ign_dump(void) {
  for (int i = 0; i < state.last_ignored; i++) {
    bool result = dump_page((off64_t) state.pages_ignored[i]);
  }
}

static void set_ignore(void) {
#ifdef _DEBUG
  printf("set_ignore()\n");
#endif
  state.dump_sa = IGNORE_SA;
  state.sa.sa_sigaction = ignore_handler;
  int res = sigaction(SIGSEGV, &state.sa, NULL);
  assert(res != -1);
}

static void set_dump(void) {
#ifdef _DEBUG
  printf("set_dump()\n");
#endif
  state.dump_sa = DUMP_SA;
  state.sa.sa_sigaction = dump_handler;
  int res = sigaction(SIGSEGV, &state.sa, NULL);
  assert(res != -1);
}

static void set_mru(void) {
#ifdef _DEBUG
  printf("set_mru()\n");
#endif
  state.dump_sa = MRU_SA;
  state.sa.sa_sigaction = mru_handler;
  int res = sigaction(SIGSEGV, &state.sa, NULL);
  assert(res != -1);
}

static void configure_sigaction(void) {
  /* Configure an alternative stack for
     the signal handler */
  stack_t ss;
  ss.ss_sp = state.hs;
  ss.ss_size = sizeof(state.hs);
  ss.ss_flags = 0;
  int res = sigaltstack(&ss, NULL);
  assert(res != -1);

  /* Configure the signal handler */
  state.sa.sa_flags = SA_SIGINFO | SA_ONSTACK; // | SA_NODEFER;
  sigemptyset(&state.sa.sa_mask);
}

void copy(char *source, char *dest) {
  char buf[BUFSIZ + 1];
  FILE *input = fopen(source, "r");
  FILE *output = fopen(dest, "w");
  if (!input || !output)
    errx(EXIT_FAILURE, "Error while copying the original binary");

  size_t bytes;

  while (0 < (bytes = fread(buf, 1, sizeof(buf), input))) {
    if (bytes != fwrite(buf, 1, bytes, output)) {
      errx(EXIT_FAILURE, "Error while copying the original binary");
    }
  }
  fclose(input);
  fclose(output);
}

static void lock_mem(void) {

#ifdef _DEBUG
  printf("lock_mem()\n");
#endif
  char *p;
  FILE *maps = fopen("/proc/self/maps", "r");

  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  char *addresses[65536];
  char buf[BUFSIZ + 1];

  size_t start, end;
  int counter = 0;
  char *start_of_stack, *end_of_stack;
  off64_t errnol = (off64_t) round_to_page(__errno_location());

  int count = 0;

  while (fgets(buf, BUFSIZ, maps)) {
    off64_t start, end;

    //fprintf(stderr, "%s", buf);
    sscanf(buf, "%lx-%lx", &start, &end);

    /* Stack is special, it should be protected last
       because we are using it */
    if (strstr(buf, "stack") != NULL) {
      end_of_stack = (char *)end;
      start_of_stack = (char *)start;
      continue;
    }
    /* Ignore libc pages */
    if (strstr(buf, "linux-gnu") != NULL)
      continue;
    /* Ignore libc special mem zones */
    if (strstr(buf, "r-xp") != NULL)
      continue;
    /* Ignore vsyscall special mem zones */
    if (strstr(buf, "vsyscall") != NULL)
      continue;

    /* Ignore libdump mem zones */
    if (strstr(buf, "libdump.so") != NULL)
      continue;

    /* Ignore libdump vdso zones */
    if (strstr(buf, "vdso") != NULL)
      continue;

    /* Ignore vvar zone (cf. https://lkml.org/lkml/2015/3/12/602)*/
    if (strstr(buf, "vvar") != NULL)
      continue;

    /* Ignore alreay protected pages */
    if (strstr(buf, "---p") != NULL)
      continue;

    char *page_start = round_to_page((char *)start);

    /* syscall() and tracing SIGSEGV handler require access to errno.  We
       must keep errno location unlocked to avoid trigering an infinite
       segfault. */

    if ((off64_t) page_start <= errnol && errnol < end) {
      /* We split the range containing errnol into two segments */

      /* If the left segment is non-empty, we must lock it */
      if ((off64_t) page_start < errnol) {
        assert(count < 65536);
        addresses[count++] = (char *)page_start;
        addresses[count++] = (char *)errnol;
      }

      /* The errnol page is added to ignored, so it is still dumped
       * even if it's kept unlocked */
      assert(state.last_ignored < MAX_IGNORE);
      state.pages_ignored[state.last_ignored++] = (char *)errnol;

      /* If the right segment is non-empty, we must lock it */
      if (errnol + PAGESIZE < end) {
        assert(count < 65536);
        addresses[count++] = (char *)(errnol + PAGESIZE);
        addresses[count++] = (char *)end;
      }
      continue;
    }

    assert(count < 65536);
    addresses[count++] = (char *)page_start;
    addresses[count++] = (char *)end;
  }

  while (count > 0) {
    char *end = addresses[--count];
    char *start = addresses[--count];
    int result = syscall(SYS_mprotect, start, end - start, PROT_NONE);
    assert(result == 0);
  }

  /* Unprotect dump state */
  int result =
      syscall(SYS_mprotect, &state, sizeof(state), PROT_READ | PROT_WRITE);
  assert(result == 0);

  /* Closing maps file must be done after locking the memory. If this close is
   * done before, it changes the memory mapping kept inside addresses making
   * the above SYS_mprotect call fail */
  fclose(maps);

  syscall(SYS_mprotect, start_of_stack, end_of_stack - start_of_stack,
          PROT_NONE);
}

static void dump_core(int count, void *addresses[]) {
  FILE *maps;
  char path[MAX_PATH];
  snprintf(path, sizeof(path), "%s/%s", state.dump_path[state.stack_pos],
           state.core_suffix);
  int i;

  FILE *core_map = fopen(path, "w");
  if (!core_map) {
    fprintf(stderr, "Could not create core.map file");
    exit(-1);
  }

  for (i = 0; i < count; i++) {
    fprintf(core_map, "%d %lx\n", i, (off64_t) addresses[i]);
  }
  fclose(core_map);
}

static void create_dump_dir(void) {
  struct stat sb;
  /* Check that dump exists or try to create it, then enter it */
  if (stat(state.dump_prefix, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(state.dump_prefix, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create dump directory");
  }
}

static void set_log_size(int log_size) {
  state.log_size = log_size;
  state.stack_pos = -1;
  state.dump_active_pos = -1;
}

void dump_init(bool global_dump) {
  state.mtrace_active = false;

  state.global_dump = global_dump;

  state.dump_prefix = strdup("cere_dumps");
  state.pagelog_suffix = strdup("hotpages.map");
  state.core_suffix = strdup("core.map");

  /* Copy the original binary */
  copy("/proc/self/exe", "lel_bin");

  /* configure atexit */
  atexit(dump_close);

  /* create dump dir */
  create_dump_dir();

  /* init counters table */
  init_counters();
  state.last_page = 0;
  state.last_trace = 0;

  /* set log size */
  set_log_size(LOG_SIZE);

  /* configure sigaction */
  configure_sigaction();

  /*If we want to dump all loops
   * start memory lock and trace at start*/
  if (state.global_dump) {
    /* set ignore */
    set_ignore();

    /* lock memory */
    lock_mem();

    /* configure mru sa */
    set_mru();

    /* start protecting malloc and co */
    state.mtrace_active = true;
  }
  state.dump_initialized = true;

#ifdef _DEBUG
  printf("DUMP_INIT DONE\n");
#endif
}

void dump_close() {
  unlink("lel_bin");

  /* deactivate memory tracing */
  state.mtrace_active = false;

  /* install ignore sa */
  set_ignore();

  /* clear the htable */
  htable_clear(&state.counters);
}

/* dumps memory
 *  loop_name: name of dumped loop
 *  invocation: the number of the invocation to dump
 *  count: number of arguments
 *  ...args...: the arguments to dump
 */
void dump(char *loop_name, int invocation, int count, ...) {
  //Avoid doing something before initializing
  //the dump.
  if (!state.dump_initialized)
    return;
#ifdef _DEBUG
  printf("enter dump( %s %d count = %d) \n", loop_name, invocation, count);
#endif
  char lel_bin_path[1024];
  /* Stop malloc protection */
  int old_state = state.mtrace_active;
  state.mtrace_active = false;

  /* Increment active dump position */
  assert(state.dump_active_pos < MAX_STACK);
  state.dump_active_pos++;

  /* get region */
  struct region_counter *region = get_region(loop_name);

  /* increment call_count */
  region->call_count++;
  /* start malloc protection PAST_INV invocations before the one we want to dump
   */
  if (!state.global_dump &&
      ((invocation <= PAST_INV && region->call_count == 1) ||
       (region->call_count == invocation - PAST_INV))) {
    /* set ignore */
    set_ignore();
    /* lock memory */
    lock_mem();
    /* configure mru sa */
    set_mru();

    /* here two possibilities 1) either the user asked to dump the current
     * invocation, (happens when invocation = 1), in which case we will
     * activate mtrace just before the dump starts at the end of this
     * function
     *
     * 2) either we are not on the good invocation in which case we have to
     * start locking mem in the next if block */
    old_state = true;
  }

  /* Did we get to the invocation that must be dumped ? */
  if (region->call_count != invocation) {
#ifdef _DEBUG
    printf("not correct invocation DUMP( %s %d count = %d) \n", loop_name,
           invocation, count);
/* reactivate malloc protection */
#endif
    state.dump_active[state.dump_active_pos] = false;
    /* restore mtrace state */
    state.mtrace_active = old_state;
    return;
  }
  printf("DUMP( %s %d count = %d) \n", loop_name, invocation, count);
  state.dump_active[state.dump_active_pos] = true;

  /* Increment stack pos */
  assert(state.stack_pos < MAX_STACK);
  int sp = state.stack_pos + 1;

  /* Ensure that the dump directory exists */
  snprintf(state.dump_path[sp], sizeof(state.dump_path[sp]), "%s/%s/",
           state.dump_prefix, loop_name);

  mkdir(state.dump_path[sp], 0777);

  snprintf(state.dump_path[sp], sizeof(state.dump_path[sp]), "%s/%s/%d",
           state.dump_prefix, loop_name, invocation);

  if (mkdir(state.dump_path[sp], 0777) != 0)
    errx(EXIT_FAILURE, "dump %s already exists, stop\n", state.dump_path[sp]);

  state.stack_pos = sp;

  assert(state.mtrace_active == false);

  /*Dump hotpages to disk*/
  flush_hot_pages_trace_to_disk();

  /*Link to the original binary*/
  snprintf(lel_bin_path, sizeof(lel_bin_path), "%s/lel_bin",
           state.dump_path[state.stack_pos]);
  int res = syscall(SYS_link, "lel_bin", lel_bin_path);
  if (res == -1)
    errx(EXIT_FAILURE, "Error copying the dump binary\n");

  /* Dump addresses */
  void *addresses[count];
  va_list ap;
  int j;
  va_start(ap, count);
  for (j = 0; j < count; j++) {
    addresses[j] = va_arg(ap, void *);
    char *start_of_page = round_to_page(addresses[j]);
    // Adresses must be dumped because in run__extracted, we
    // dereference scalar ones.
    if (start_of_page != 0) {
      dump_page((off64_t) start_of_page);
    }
  }
  va_end(ap);
  dump_core(count, addresses);

  /* Dump ignored pages */
  page_ign_dump();

  /* Dump unprotected pages */
  dump_unprotected_pages();

  /* set dump sa */
  set_dump();

  /* Reactivate mem */
  state.mtrace_active = true;
  /*If we have dumped the requested invocation
   * and we are not in global dumping mode
   * no need to continue execution*/
  if (region->call_count == invocation && !state.global_dump)
    state.kill_after_dump = true;
}

void after_dump(void) {
  //Avoid doing something before initializing
  //the dump.
  if (!state.dump_initialized)
    return;

  if (state.global_dump) {
    assert(state.mtrace_active == true);
    assert(state.dump_sa == MRU_SA || state.dump_sa == DUMP_SA);
  }
  if (state.dump_active[state.dump_active_pos] == true) {
    state.stack_pos--;
    assert(state.stack_pos >= -1);
    if (state.stack_pos == -1) {
      set_mru();
    }
  }
  state.dump_active_pos--;
  assert(state.dump_active_pos >= -1);

  if (state.kill_after_dump)
    exit(EXIT_SUCCESS);
}
