/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2015, Universite de Versailles St-Quentin-en-Yvelines  *
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
#include <sys/prctl.h>

#include "dump.h"
#include "counters.h"
#include "err.h"
#include "ptrace.h"
#include "syscall_interface.h"

#define _DEBUG 1
#undef _DEBUG

struct dump_state state __attribute__((aligned(PAGESIZE)));
extern const char *__progname;

void create_dump_dir(void) {
  struct stat sb;
  /* Check that dump exists or try to create it, then enter it */
  if (stat(state.dump_prefix, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(state.dump_prefix, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create %s %s", state.dump_prefix, strerror(errno));
  }

  char dump_root[MAX_PATH];
  snprintf(dump_root, sizeof(dump_root), "%s/%s",
           state.dump_prefix, state.dump_root);

  if (stat(dump_root, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(dump_root, 0777) != 0)
        errx(EXIT_FAILURE, "Could not create %s: %s", dump_root, strerror(errno));
  }
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


void lock_mem() {
  sigtrap();
}

void dump_init(void) {
  
  state.dump_prefix = strdup(".cere");
  state.dump_root = strdup("dumps");
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

  pid_t child = 0;
  int status = 0;

  child = fork();

  if (child == (pid_t)-1) {
    errx(EXIT_FAILURE, "fork() failed: %s.\n", strerror(errno));
  }

  /* If we are the parent */
  if (child != 0) {
    fflush(stdout);
    fflush(stderr);
    execlp("./tracer", "./tracer", child, (char*)NULL);
    return;
  } 

  prctl(PR_SET_DUMPABLE, (long)1);
  prctl(PR_SET_PTRACER, (long)getppid());
 
  state.dump_initialized = true;

#ifdef _DEBUG
  printf("DUMP_INIT DONE\n");
#endif

}

void dump_close() {
  unlink("lel_bin");
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

  sigtrap();
  
  fake_syscall(loop_name);
  fake_syscall(invocation);
  fake_syscall(count);

  bool good_invocation;
  fake_syscall(&good_invocation);

  if (good_invocation) {
    /* Dump addresses */
    int i;
    va_list ap;
    va_start(ap, count);
    for (i = 0; i < count; i++) {
      fake_syscall(va_arg(ap, void *));
    }
    va_end(ap);

    state.kill_after_dump = true;
  }
}

void after_dump(void) {
  //Avoid doing something before initializing
  //the dump.
  if (!state.dump_initialized)
    return;

  if (state.kill_after_dump)
    exit(EXIT_SUCCESS);
}
