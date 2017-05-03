/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2016-2017, Universite de Versailles St-Quentin-en-Yvelines  *
 *                                                                           *
 * CERE is free software: you can redistribute it and/or modify it under     *
 * the terms of the GNU Lesser General Public License as published by        *
 * the Free Software Foundation, either version 3 of the License,            *
 * or (at your option) any later version.                                    *
 *                                                                           *
 * CERE is distributed in the hope that it will be useful,                   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 * GNU General Public License for more details.                              *
 *                                                                           *
 * You should have received a copy of the GNU General Public License         *
 * along with CERE.  If not, see <http://www.gnu.org/licenses/>.             *
 *****************************************************************************/
#include <assert.h>
#include <fcntl.h>
#include <err.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <syscall.h>
#include <unistd.h>

#include <ccan/hash/hash.h>
#include <ccan/htable/htable.h>
#include "pages.h"
#include "ptrace.h"
#include "tracer_interface.h"
#include "types.h"

#define _DEBUG 1
#undef _DEBUG


#include "debug.h"

long PAGESIZE;
int log_size = LOG_SIZE;
int last_trace = 0;
int last_page = 0;
char *dump_prefix = NULL;
const char *firsttouch_suffix = "firsttouch.map";
const char *pagelog_suffix = "hotpages.map";
const char *core_suffix = "core.map";
const char *dump_root = "dumps";
const char *replay_root = "replays";

char *pages_cache[LOG_SIZE];
char dump_path[MAX_PATH];
char *pages_trace[TRACE_SIZE];

char *cere_errors_EIO = "IO not replayable";

enum tracer_state_t tracer_state = 0;

struct tracer_buff_t * tracer_buff;

/* The loop name and invocation, these are received from the tracee
   in the tracer_dump function
*/
char loop_name[SIZE_LOOP];
int invocation = -1;

/* This hash table keeps the first touch of each page */

bool firsttouch_active = false;

static struct htable firsttouch;
typedef struct {
  int tid;
  void * start_of_page;
} ft_entry;

static size_t rehash (const void *e, void *unused) {
  const ft_entry * ft = (const ft_entry *) e;
  return hash_pointer(ft->start_of_page, 0);
}

static bool ptrequ(const void *e, void *f) {
  const ft_entry * ft = (const ft_entry *) e;
  return  ft->start_of_page == f;
}

static void tracer_lock_range(pid_t child);

static void read_map(pid_t pid) {
  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);

  FILE *maps = fopen(maps_path, "r");
  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  char buf[BUFSIZ + 1];
  while (fgets(buf, BUFSIZ, maps))
    fprintf(stderr, "%s", buf);

  fclose(maps);
}

static bool is_valid_io(pid_t pid) {
  int syscallid = get_syscallid(pid);
  int fd;

  switch (syscallid) {
  case SYS_write:
    fd = get_arg_from_regs(pid);
    return (fd == fileno(stdout) || fd == fileno(stderr));
    // All other IOs are forbidden
  case SYS_read:
#if defined(__amd64__)
  case SYS_open:
#endif
  case SYS_openat:
  case SYS_close:
    return false;
  default:
    return true;
  }
}

static bool is_syscall_io(pid_t pid) {
  int syscallid = get_syscallid(pid);
  switch (syscallid) {
  case SYS_read:
  case SYS_write:
#if defined(__amd64__)
  case SYS_open:
#endif
  case SYS_openat:
  case SYS_close:
    debug_print("Syscall IO detected : %d\n", syscallid);
   return true;
  default:
    return false;
  }
}

static bool is_mru(void *addr) {
  char *start_of_page = round_to_page(addr);
  bool present = false;
  for (int i = 0; i < log_size; i++) {
    if (pages_cache[i] == start_of_page) {
      present = true;
      break;
    }
  }
  return present;
}


static void dump_firsttouch(void) {
  debug_print("%s", "Dump firsttouch.map\n");

  char path[MAX_PATH];
  snprintf(path, sizeof(path), "%s/%s", dump_path, firsttouch_suffix);

  FILE *ft_map = fopen(path, "w");
  if (!ft_map)
    errx(EXIT_FAILURE, "Could not create %s file : %s\n",
         firsttouch_suffix, strerror(errno));

  struct htable_iter iter;
  ft_entry * t;
  for (t = htable_first(&firsttouch, &iter);
       t;
       t = htable_next(&firsttouch, &iter)) {
    fprintf(ft_map, "%d %lx\n", t->tid, (unsigned long)t->start_of_page);
  }
  fclose(ft_map);
}



static void dump_core(const size_t naddr, void *addresses[naddr]) {
  debug_print("%s", "Dump core\n");

  char path[MAX_PATH];
  snprintf(path, sizeof(path), "%s/%s", dump_path, core_suffix);

  FILE *core_map = fopen(path, "w");
  if (!core_map)
    errx(EXIT_FAILURE, "Could not create core.map file : %s\n",
         strerror(errno));

  for (unsigned int i = 0; i < naddr; i++)
    fprintf(core_map, "%d %lx\n", i, (register_t)addresses[i]);
  fclose(core_map);
}

static void dump_page(pid_t pid, void *start) {
  debug_print("Dump page %p\n", start);

  char current_path[MAX_PATH];
  snprintf(current_path, sizeof(current_path), "%s/%012lx.memdump", dump_path,
           (register_t)start);

  ptrace_putdata(pid, (long)tracer_buff->str_tmp, current_path, MAX_PATH);
  if (access(current_path, F_OK) != -1)
    return;

  int out = openat_i(pid, tracer_buff->str_tmp);
  write_i(pid, out, start, PAGESIZE);
  close_i(pid, out);
}

static void dump_handler(int pid, void *start_of_page) {
  debug_print("Dump handler detected access at %p\n", start_of_page);

  /* Unprotect Page */
  unprotect_i(pid, start_of_page, PAGESIZE);
  dump_page(pid, start_of_page);
}

static void register_first_touch(int pid, void * start_of_page) {
  size_t hash = hash_pointer(start_of_page, 0);
  /* Is this the first time we touch this page ? */
  ft_entry * t = htable_get(&firsttouch, hash, ptrequ, start_of_page);

  /* If not record the touching thread to the firsttouch htable */
  if (!t) {
    t = malloc(sizeof(ft_entry));
    t->tid = pid;
    t->start_of_page = start_of_page;
    htable_add(&firsttouch, hash, t);
    debug_print("First touch by %d detected at %p\n", pid, start_of_page);
  }

  ft_entry * x = htable_get(&firsttouch, hash, ptrequ, start_of_page);
}

static void firsttouch_handler(int pid, void *start_of_page) {
  unprotect_i(pid, start_of_page, PAGESIZE);
  register_first_touch(pid, start_of_page);
}

static void mru_handler(int pid, void *start_of_page) {
  debug_print("MRU Detected access at %p from %d\n", start_of_page, pid);

  bool present = is_mru(start_of_page);
  if (present) {
    /* In parallel applications it may happen that two segfaults on the same
       page are queued. The first handler will unprotect the page, and add it to
       mru, therefore there is nothing to do in the second handler. */
    return;
  }

  if (firsttouch_active) {
    register_first_touch(pid, start_of_page);
  }

  if (pages_cache[last_page] != 0) {
    /* add the evicted page to the trace */
    pages_trace[last_trace] = pages_cache[last_page];
    last_trace = (last_trace + 1) % TRACE_SIZE;

    debug_print("MRU Reprotecting page %p\n", pages_cache[last_page]);

    unprotect_protect_i(pid, start_of_page, PAGESIZE, pages_cache[last_page],
                      PAGESIZE);

  } else {
    unprotect_i(pid, start_of_page, PAGESIZE);
  }

  pages_cache[last_page] = start_of_page;
  last_page = (last_page + 1) % log_size;

  /*
#ifdef _DEBUG
  debug_print("%s", "     >>>> page cache\n");
  for (int i = 0; i < log_size; i++) {
    int c = (i + last_page) % log_size;
    debug_print("     >>>> %p\n", pages_cache[c]);
  }
#endif
  */
}

static void mark_invalid() {
  char buff[BUFSIZ];
  snprintf(buff, sizeof(buff), "%s/%s/invalid_regions", dump_prefix,
           replay_root);
  FILE *ir_file = fopen(buff, "a");
  if (!ir_file)
    errx(EXIT_FAILURE, "Error creating %s : %s\n", buff, strerror(errno));
  fprintf(ir_file, "%s %s\n", loop_name, cere_errors_EIO);
  fclose(ir_file);

  snprintf(buff, sizeof(buff), "rm -rf %s/%s/%s/%d", dump_prefix, dump_root,
           loop_name, invocation);
  int ret = system(buff);
  if (ret == -1)
    errx(EXIT_FAILURE, "Error deleting %s : %s\n", buff, strerror(errno));

  errx(EXIT_FAILURE, "%s", cere_errors_EIO);
}

/* For each syscall we get called twice: before and after entering the
   kernel handler. In aarch64 orig_x0 does not exist, therefore we have
   to ignore the second call. sc_pending is true for a given pid, if
   the second call has not yet been cleared.
*/
bool sc_pending[MAX_TIDS] = {false};

static void handle_syscall(pid_t child) {
  if (sc_pending[child]) {
    sc_pending[child] = false;
    return;
  }
  else {
    sc_pending[child] = true;
  }
  /* IO Syscall require special care */
  if (is_syscall_io(child)) {
    if (tracer_state == TRACER_DUMPING && !is_valid_io(child)) {
      mark_invalid();
    }
  }
}

pid_t handle_events_until_dump_trap(pid_t wait_for) {
  while (true) {
    event_t e = wait_event(wait_for);
    if (e.signo == SIGTRAP) {
      if (is_dump_sigtrap(e.tid)) {
        clear_trap(e.tid);
        return e.tid;
      } else if (is_hook_sigtrap(e.tid)) {
        assert(tracer_state == TRACER_LOCKED || tracer_state == TRACER_FIRSTTOUCH);
        clear_trap(e.tid);
        tracer_lock_range(e.tid);
        continue;
      } else { // If we arrive here, it is a syscall
        handle_syscall(e.tid);
        ptrace_syscall(e.tid);
        continue;
      }
    } else if (e.signo == SIGSEGV) {
      void *addr = round_to_page(e.sigaddr);
      switch (tracer_state) {
      case TRACER_UNLOCKED:
        /* We should never get a sigsegv in unlocked state ! */
        errx(EXIT_FAILURE,
             "SIGSEGV at %p before locking memory during capture\n",
             e.sigaddr);
      case TRACER_FIRSTTOUCH:
        firsttouch_handler(e.tid, addr);
        break;
      case TRACER_LOCKED:
        mru_handler(e.tid, addr);
        break;
      case TRACER_DUMPING:
        dump_handler(e.tid, addr);
        break;
      default:
        assert(false); /* we should never be here */
      }
      ptrace_syscall(e.tid);
    }
    else if (e.signo == SIGSTOP) {
      /* A new thread is starting, ignore this event, next wait_event call will
         unblock the thread once its parents registers it in tids array */
    }
    else if (e.signo == SIGWINCH) {
      /* Ignore signal SIGWINCH, tty resize */
      ptrace_syscall(e.tid);
      continue;
    }
    else {
      errx(EXIT_FAILURE, "Unexpected signal in wait_sigtrap: %d\n", e.signo);
    }
  }
  debug_print("%s", "\n");
}

static register_t receive_from_tracee(pid_t child) {
  register_t ret;
  debug_print("receive from tracee %d\n", child);

  /* we should only handle events from the child that started
  the communication here; we should be in safe
  non blockable code (send_to_tracee) so we do not care about
  other threads. Answering to a dump trap from another process
  here could lead to a synchronization problem */
  pid_t tid = handle_events_until_dump_trap(child);
  ret = get_arg_from_regs(tid);
  return ret;
}

static void tracer_lock_range(pid_t child) {

  debug_print("%s %d\n", "START LOCK RANGE", child);

  assert(tracer_state == TRACER_LOCKED);

  ptrace_syscall(child);
  void *from = (void *)receive_from_tracee(child);
  ptrace_syscall(child);
  void *to = (void *)receive_from_tracee(child);

  /* We need that the process be stopped to protect  */

  long unsigned nb_pages_to_allocate = nb_pages_in_range(from, to);

  for (long unsigned i = 0; i < nb_pages_to_allocate; i++)
    if (!is_mru(from + PAGESIZE * i))
      protect_i(child, round_to_page(from + PAGESIZE * i), PAGESIZE);

  ptrace_syscall(child);

  debug_print("%s %d (%p -> %p)\n", "END LOCK RANGE", child, from, to);
}

static void create_root_dir(const char * root) {
  struct stat sb;
  char dump_root_[MAX_PATH];
  snprintf(dump_root_, sizeof(dump_root_), "%s/%s", dump_prefix, root);

  if (stat(dump_root_, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(dump_root_, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create %s: %s", dump_root_,
           strerror(errno));
  }
}

static void create_dump_dir(void) {
  struct stat sb;
  /* Check that dump exists or try to create it, then enter it  */
  if (stat(dump_prefix, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(dump_prefix, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create %s %s", dump_prefix,
           strerror(errno));
  }

  create_root_dir(dump_root);
  create_root_dir(replay_root);
}

static void tracer_lock_mem(pid_t pid) {

  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);
  FILE *maps = fopen(maps_path, "r");

  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  debug_print("%s\n", "START LOCK MEM");

  void *addresses[65536];
  char buf[BUFSIZ + 1];
  int counter = 0;

  while (fgets(buf, BUFSIZ, maps)) {
    void *start, *end;

    debug_print("%s", buf);
    sscanf(buf, "%p-%p", &start, &end);

    /* Ignore libdump mem zones  */
    if (strstr(buf, "libcere_dump.so") != NULL)
      continue;

    /* Ignore libc pages  */
    if (strstr(buf, "linux-gnu") != NULL)
      continue;

    /* Ignore libc special mem zones  */
    /* If we don't ignore those mem zones we get this error */
    /* /usr/bin/ld: la section .interp chargée à  */
    /*    [00000000004003c0 -> 00000000004003db]  */
    /*    chevauche la section s000000400000 chargée à  */
    /*    [0000000000400000 -> 0000000000400fff] */
    if (strstr(buf, "r-xp") != NULL)
      continue;

    /* Ignore vsyscall special mem zones  */
    if (strstr(buf, "vsyscall") != NULL)
      continue;

    /* Ignore libdump vdso zones  */
    if (strstr(buf, "vdso") != NULL)
      continue;

    /* Ignore vvar zone (cf. https://lkml.org/lkml/2015/3/12/602) */
    if (strstr(buf, "vvar") != NULL)
      continue;

    /* Ignore alreay protected pages  */
    if (strstr(buf, "---p") != NULL)
      continue;

    assert(counter < 65536);
    addresses[counter++] = round_to_page(start);
    addresses[counter++] = end;
  }

  /* Protect all pages in adresses */
  while (counter > 0) {
    void *end = addresses[--counter];
    void *start = addresses[--counter];
    protect_i(pid, start, (end - start));
  }

  int r = fclose(maps);
  if (r != 0)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ %s\n",
         strerror(errno));

  debug_print("%s\n", "END LOCK MEM");
}

static void flush_hot_pages_trace_to_disk(pid_t pid) {

  debug_print("%s\n", "START FLUSH HOT PAGE");

  char path[MAX_PATH];
  char buf[14];

  snprintf(path, sizeof(path), "%s/%s", dump_path, pagelog_suffix);
  FILE *out = fopen(path, "w");
  if (out == 0)
    errx(EXIT_FAILURE, "Error open %s : %s\n", path, strerror(errno));

  for (int i = 0; i < TRACE_SIZE; i++) {
    int c = (i + last_trace) % TRACE_SIZE;
    if (pages_trace[c] != 0) {
      fprintf(out, "%p\n", pages_trace[c]);
    }
  }

  /* Dump the log */
  for (int i = 0; i < log_size; i++) {
    int c = (i + last_page) % log_size;
    if (pages_cache[c] != 0) {
      fprintf(out, "%p\n", pages_cache[c]);
    }
  }

  fclose(out);

  debug_print("%s\n", "END FLUSH HOT PAGE");
}

static void dump_unprotected_pages(pid_t pid) {
  debug_print("%s\n", "START DUMP UNPROTECTED PAGE");

  /* Dump the unprotected pages before entering the codelet region. */
  for (int i = 0; i < log_size; i++) {
    int c = (i + last_page) % log_size;
    if (pages_cache[c] != 0) {
      dump_page(pid, pages_cache[c]);
    }
  }

  debug_print("%s\n", "END DUMP UNPROTECTED PAGE");
}

static void tracer_dump(pid_t pid) {

  /* Read arguments from tracee */
  handle_events_until_dump_trap(-1);
  register_t ret = get_arg_from_regs(pid);
  assert(ret == TRAP_START_ARGS);

  debug_print("receive string from tracee %d\n", pid);
  ptrace_getdata(pid, (long) tracer_buff->str_tmp, loop_name, SIZE_LOOP);
  ptrace_syscall(pid);

  invocation = (int)receive_from_tracee(pid);
  ptrace_syscall(pid);

  int arg_count = (int)receive_from_tracee(pid);
  ptrace_syscall(pid);

  printf("DUMP( %s %d count = %d) \n", loop_name, invocation, arg_count);

  /* Ensure that the dump directory exists */
  snprintf(dump_path, sizeof(dump_path), "%s/%s/%s", dump_prefix, dump_root,
           loop_name);

  mkdir(dump_path, 0777);

  snprintf(dump_path, sizeof(dump_path), "%s/%s/%s/%d", dump_prefix, dump_root,
           loop_name, invocation);

  if (mkdir(dump_path, 0777) != 0)
    errx(EXIT_FAILURE, "dump %s already exists, stop\n", dump_path);

  int i;
  void *addresses[arg_count];
  for (i = 0; i < arg_count; i++) {
    addresses[i] = (void *)receive_from_tracee(pid);
    ptrace_syscall(pid);
  }

  /* Wait for end of arguments sigtrap */
  handle_events_until_dump_trap(pid);
  ret = get_arg_from_regs(pid);
  assert(ret == TRAP_END_ARGS);

  /* Dump hotpages to disk */
  flush_hot_pages_trace_to_disk(pid);

  char lel_bin_path[1024];
  /* Link to the original binary */
  snprintf(lel_bin_path, sizeof(lel_bin_path), "%s/lel_bin", dump_path);
  int res =
      linkat(AT_FDCWD, "lel_bin", AT_FDCWD, lel_bin_path, AT_SYMLINK_FOLLOW);
  if (res == -1)
    errx(EXIT_FAILURE, "Error copying the dump binary\n");

  for (i = 0; i < arg_count; i++) {
    void *start_of_page = round_to_page(addresses[i]);
    if (start_of_page != NULL) {
      unprotect_i(pid, start_of_page, PAGESIZE);
      dump_page(pid, start_of_page);
    }
  }

  if (firsttouch_active) {
    dump_firsttouch();
  }

  dump_core(arg_count, addresses);
  dump_unprotected_pages(pid);
}

static void tracer_init(pid_t pid) {
  PAGESIZE = sysconf(_SC_PAGESIZE);
  event_t e = wait_event(pid);
  assert(e.signo == SIGSTOP);
  follow_threads(pid);
  create_dump_dir();

  debug_print("%s\n", "Tracer initialized");

  if (firsttouch_active) {
    htable_init(&firsttouch, rehash, NULL);
    stop_all_except(pid);
    tracer_lock_mem(pid);
    debug_print("%s\n", "******* TRACER_FIRSTTOUCH");
    tracer_state = TRACER_FIRSTTOUCH;
  } else {
    debug_print("%s\n", "******* TRACER_UNLOCKED");
    tracer_state = TRACER_UNLOCKED;
  }

  continue_all();
}

int main(int argc, char *argv[]) {
  pid_t child = 0;
  siginfo_t sig;

  if (argc != 3) {
    errx(EXIT_FAILURE, "usage: %s pid tracer_buff_address\n", argv[0]);
  }

  dump_prefix = getenv("CERE_WORKING_PATH");
  if(!dump_prefix) {
    debug_print("%s\n", "CERE_WORKING_PATH not defined, using defaut cere dir.\n");
    dump_prefix = ".cere";
  }

  char * ft = getenv("CERE_FIRSTTOUCH");
  if (ft && strcmp("TRUE", ft) == 0) {
    firsttouch_active = true;
    debug_print("%s\n", "First touch capture is active");
  }

  child = atoi(argv[1]);
  sscanf(argv[2], "%p", &tracer_buff);

  tracer_init(child);

  /* Wait for lock_mem trap */
  pid_t tid = handle_events_until_dump_trap(-1);
  register_t ret = get_arg_from_regs(tid);
  assert(ret == TRAP_LOCK_MEM);

  stop_all_except(tid);
  tracer_lock_mem(tid);
  debug_print("%s\n", "******* TRACER_LOCKED");
  tracer_state = TRACER_LOCKED;
  continue_all();

  /* Dump arguments */
  tracer_dump(tid);

  debug_print("%s\n", "******* TRACER_DUMPING");
  tracer_state = TRACER_DUMPING;
  ptrace_syscall(tid);

  while (1) {
    handle_events_until_dump_trap(-1);
  }
}
