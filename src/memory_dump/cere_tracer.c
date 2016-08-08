/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2016, Universite de Versailles St-Quentin-en-Yvelines       *
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
#include <unistd.h>

#include "pages.h"
#include "ptrace.h"
#include "syscall_interface.h"
#include "types.h"

#define _DEBUG 1
//#undef _DEBUG

#include "debug.h"

int log_size = LOG_SIZE;
int last_trace = 0;
int last_page = 0;
const char *pagelog_suffix = "hotpages.map";
const char *core_suffix = "core.map";
const char *dump_prefix = ".cere";
const char *dump_root = "dumps";
const char *replay_root = "replays";
char *pages_cache[LOG_SIZE];
char dump_path[MAX_PATH];
char *pages_trace[TRACE_SIZE];

char loop_name[SIZE_LOOP];
int invocation = -1;
int count;

char *cere_errors_EIO = "IO not replayable";

enum tracer_state_t tracer_state = 0;

struct tracer_buff_t tracer_buff;

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

static void write_binary(pid_t pid, int out, void *in, int nbyte) {
  write_page(pid, out, in, nbyte);
}

static void dump_core(int count, void *addresses[]) {
  debug_print("%s", "DUMP CORE\n");

  char path[MAX_PATH];
  snprintf(path, sizeof(path), "%s/%s", dump_path, core_suffix);

  FILE *core_map = fopen(path, "w");
  if (!core_map)
    errx(EXIT_FAILURE, "Could not create core.map file : %s\n",
         strerror(errno));

  int i;
  for (i = 0; i < count; i++)
    fprintf(core_map, "%d %lx\n", i, (register_t)addresses[i]);
  fclose(core_map);

  debug_print("%s", "END DUMP CORE\n");
}

static void dump_page(pid_t pid, void *start) {
  debug_print("Dump page %p\n", start);

  char current_path[MAX_PATH];
  snprintf(current_path, sizeof(current_path), "%s/%012lx.memdump", dump_path,
           (register_t)start);

  put_string(pid, current_path, tracer_buff.str_tmp, MAX_PATH);
  if (access(current_path, F_OK) != -1)
    return;

  int out = openat_i(pid, tracer_buff.str_tmp);
  write_binary(pid, out, start, PAGESIZE);
  close_i(pid, out);
}

static void dump_handler(int pid, void *start_of_page) {
  debug_print("Dump handler detected access at %p\n", start_of_page);

  /* Unprotect Page */
  unprotect(pid, start_of_page, PAGESIZE);
  dump_page(pid, start_of_page);
}

static void mru_handler(int pid, void *start_of_page) {
  debug_print("MRU Detected access at %p\n", start_of_page);

  bool present = is_mru(start_of_page);
  if (present) {
    return;
  }
  //assert(present == false);

  if (pages_cache[last_page] != 0) {
    /* add the evicted page to the trace */
    pages_trace[last_trace] = pages_cache[last_page];
    last_trace = (last_trace + 1) % TRACE_SIZE;

    debug_print("MRU Reprotecting page %p\n", pages_cache[last_page]);

    unprotect_protect(pid, start_of_page, PAGESIZE, pages_cache[last_page],
                      PAGESIZE);

  } else {
    unprotect(pid, start_of_page, PAGESIZE);
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

static void handle_syscall(pid_t child) {
  /* IO Syscall require special care */
  if (is_syscall_io(child)) {
    if (tracer_state == TRACER_DUMPING && !is_valid_io(child)) {
      mark_invalid();
    }
  }
}

/* Catch all for other syscalls (not IO ones)*/
static pid_t wait_sigtrap(pid_t wait_for, siginfo_t * sig) {
  while (true) {
    pid_t pid = wait_process(wait_for, sig);
    if (sig->si_signo == SIGTRAP || sig->si_signo == (SIGTRAP | 0x80)) {
      if (tracer_state == TRACER_LOCKED && is_hook_sigtrap(pid)) {
        tracer_lock_range(pid);
        continue;
      } else if (is_dump_sigtrap(pid)) {
        return pid;
      } else { // If we arrive here, it is a syscall
        handle_syscall(pid);
        ptrace_syscall(pid);
        continue;
      }
    } else if (sig->si_signo == SIGSEGV) {
      void *addr = round_to_page(sig->si_addr);
      switch (tracer_state) {
      case TRACER_UNLOCKED:
        /* We should never get a sigsegv in unlocked state ! */
        errx(EXIT_FAILURE,
             "SIGSEGV at %p before locking memory during capture\n",
             sig->si_addr);
      case TRACER_LOCKED:
        mru_handler(pid, addr);
        break;
      case TRACER_DUMPING:
        dump_handler(pid, addr);
        break;
      default:
        assert(false); /* we should never be here */
      }
      ptrace_syscall(pid);
    } else if (sig->si_signo == SIGSTOP) {
      debug_print("%s\n", "Cleared SIGSTOP");
      // Non cleared SIGSTOP after a stop-all
      continue;
    } else {
      errx(EXIT_FAILURE, "Error after lock_mem and before dump %s\n",
           strerror(sig->si_signo));
    }
  }
  debug_print("%s", "\n");
}

static register_t receive_from_tracee(pid_t child) {
  register_t ret;
  siginfo_t sig;
  debug_print("receive from tracee %d\n", child);
  wait_sigtrap(child, &sig);
  assert(sig.si_signo == SIGTRAP || sig.si_signo == (SIGTRAP | 0x80));
  ret = get_arg_from_regs(child);
  ptrace_syscall(child);
  return ret;
}

static void receive_string_from_tracee(pid_t child, char *src_tracee,
                                       void *dst_tracer, size_t size) {
  siginfo_t sig;
  debug_print("receive string from tracee %d\n", child);
  wait_sigtrap(-1, &sig);
  ptrace_getdata(child, (long)src_tracee, dst_tracer, size);
  ptrace_syscall(child);
  debug_print("DONE receiving string from tracee\n", child);
}

static void tracer_lock_range(pid_t child) {

  debug_print("%s\n", "START LOCK RANGE");

  assert(tracer_state == TRACER_LOCKED);

  ptrace_syscall(child);
  void *from = (void *)receive_from_tracee(child);
  void *to = (void *)receive_from_tracee(child);
  /* We need that the process be stopped to protect  */
  siginfo_t sig;
  wait_process(child, &sig);

  long unsigned nb_pages_to_allocate = nb_pages_in_range(from, to);

  for (long unsigned i = 0; i < nb_pages_to_allocate; i++)
    if (!is_mru(from + PAGESIZE * i))
      protect(child, round_to_page(from + PAGESIZE * i), PAGESIZE);

  ptrace_syscall(child);

  debug_print("%s\n", "END LOCK RANGE");
}

static void create_dump_dir(void) {
  struct stat sb;
  /* Check that dump exists or try to create it, then enter it  */
  if (stat(dump_prefix, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(dump_prefix, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create %s %s", dump_prefix,
           strerror(errno));
  }

  char dump_root_[MAX_PATH];
  snprintf(dump_root_, sizeof(dump_root_), "%s/%s", dump_prefix, dump_root);

  if (stat(dump_root_, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(dump_root_, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create %s: %s", dump_root_,
           strerror(errno));
  }
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
    protect(pid, start, (end - start));
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

static void dump_arg(pid_t pid) {

  printf("DUMP( %s %d count = %d) \n", loop_name, invocation, count);

  /* Ensure that the dump directory exists */
  snprintf(dump_path, sizeof(dump_path), "%s/%s/%s", dump_prefix, dump_root,
           loop_name);

  mkdir(dump_path, 0777);

  snprintf(dump_path, sizeof(dump_path), "%s/%s/%s/%d", dump_prefix, dump_root,
           loop_name, invocation);

  if (mkdir(dump_path, 0777) != 0)
    errx(EXIT_FAILURE, "dump %s already exists, stop\n", dump_path);

  int i;
  void *addresses[count];
  for (i = 0; i < count; i++)
    addresses[i] = (void *)receive_from_tracee(pid);

  /* Wait for end of arguments sigtrap */
  siginfo_t sig;
  wait_sigtrap(pid, &sig);

  /* Dump hotpages to disk */
  flush_hot_pages_trace_to_disk(pid);

  char lel_bin_path[1024];
  /* Link to the original binary */
  snprintf(lel_bin_path, sizeof(lel_bin_path), "%s/lel_bin", dump_path);
  int res =
      linkat(AT_FDCWD, "lel_bin", AT_FDCWD, lel_bin_path, AT_SYMLINK_FOLLOW);
  if (res == -1)
    errx(EXIT_FAILURE, "Error copying the dump binary\n");

  for (i = 0; i < count; i++) {
    void *start_of_page = round_to_page(addresses[i]);
    if (start_of_page != NULL) {
      unprotect(pid, start_of_page, PAGESIZE);
      dump_page(pid, start_of_page);
    }
  }

  dump_core(count, addresses);
  dump_unprotected_pages(pid);
}

static void read_args(pid_t pid) {

  receive_string_from_tracee(pid, tracer_buff.str_tmp, loop_name, SIZE_LOOP);
  invocation = (int)receive_from_tracee(pid);
  count = (int)receive_from_tracee(pid);

  debug_print("LOOP NAME : %s\n", loop_name);
  debug_print("INVOCATION : %d\n", invocation);
  debug_print("COUNT : %d\n", count);
}

static void tracer_dump(pid_t pid) {
  read_args(pid);
  dump_arg(pid);
}

static void tracer_init(pid_t pid) {
  siginfo_t sig;
  wait_process(pid, &sig);
  ptrace_attach(pid);
  create_dump_dir();
  ptrace_syscall(pid);

  debug_print("%s\n", "TRACER INIT DONE");
}

int main(int argc, char *argv[]) {

  pid_t child = 0;
  siginfo_t sig;

  child = atoi(argv[1]);
  sscanf(argv[2], "%p", &tracer_buff.syscall);
  sscanf(argv[3], "%p", &tracer_buff.unprotect_protect);
  sscanf(argv[4], "%p", &tracer_buff.str_tmp);

  tracer_init(child);
  debug_print("%s\n", "******* TRACER_UNLOCKED");
  tracer_state = TRACER_UNLOCKED;

  /* Wait for lock_mem trap */
  pid_t locking_pid = wait_sigtrap(-1, &sig);
  debug_print("%s\n", "******* Start Locking");


  stop_all_except(locking_pid);
  tracer_lock_mem(locking_pid);
  debug_print("%s\n", "******* TRACER_LOCKED");
  tracer_state = TRACER_LOCKED;

  continue_all();

  //ptrace_syscall(child);

  /* Dump arguments */
  tracer_dump(locking_pid);

  debug_print("%s\n", "******* TRACER_DUMPING");
  tracer_state = TRACER_DUMPING;
  continue_all();

  while (1) {
    wait_sigtrap(-1, &sig);
  }

  ptrace_detach(child);

  return EXIT_SUCCESS;
}
