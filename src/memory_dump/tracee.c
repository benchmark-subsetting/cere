/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2017, Universite de Versailles St-Quentin-en-Yvelines  *
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
#include <errno.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <unistd.h>

#include "err.h"
#include "pages.h"
#include "types.h"
#include "tracee.h"
#include "tracee_interface.h"

#define _DEBUG 1
#undef _DEBUG

#include "debug.h"

static int times_called = 0;
static bool dump_initialized;
volatile static bool kill_after_dump = false;

void *(*real_malloc)(size_t);
void *(*real_calloc)(size_t nmemb, size_t size);
void *(*real_realloc)(void *ptr, size_t size);
void *(*real_memalign)(size_t alignment, size_t size);
bool mtrace_active;
long PAGESIZE;

void dump_init(void) {

  PAGESIZE = sysconf(_SC_PAGESIZE);

  /* state.mtrace_active = false; */
  mtrace_active = false;

  /* Copy the original binary */
  char buf[BUFSIZ];
  snprintf(buf, sizeof buf, "cp /proc/%d/exe lel_bin", getpid());
  int ret = system(buf);
  if (ret != 0) {
    errx(EXIT_FAILURE, "lel_bin copy failed: %s.\n", strerror(errno));
  }

  /* configure atexit */
  atexit(dump_close);

  pid_t child = 0;

  child = fork();

  if (child == (pid_t)-1) {
    errx(EXIT_FAILURE, "fork() failed: %s.\n", strerror(errno));
  }

  /* If we are the parent */
  if (child != 0) {

    char args[2][32];
    snprintf(args[0], sizeof(args[0]), "%d", child);
    snprintf(args[1], sizeof(args[1]), "%p", &tracer_buff);

    char *const arg[] = {"cere-tracer", args[0], args[1], NULL};
    execvp("cere-tracer", arg);
    errx(EXIT_FAILURE, "ERROR TRACER RUNNING : %s\n", strerror(errno));
  } else {

    /* Give DUMPABLE capability, required by ptrace */
    if (prctl(PR_SET_DUMPABLE, (long)1) != 0) {
      errx(EXIT_FAILURE, "Prctl : %s\n", strerror(errno));
    }

    /* Make tracer buff executable */
    if (mprotect(round_to_page(&tracer_buff), PAGESIZE,
                 (PROT_READ | PROT_WRITE | PROT_EXEC)) != 0) {
      errx(EXIT_FAILURE, "Failed to make tracer buff executable : %s\n",
           strerror(errno));
    }

    /* Request trace */
    if (ptrace(PTRACE_TRACEME, 0, 0, 0) == -1) {
      errx(EXIT_FAILURE, "ptrace(PTRACE_ME) : %s\n", strerror(errno));
    }

    debug_print("requesting ptrace from %d\n", getpid());
    raise(SIGSTOP);

    dump_initialized = true;
  }

  debug_print("%s", "DUMP_INIT DONE\n");
}

void dump_close() { unlink("lel_bin"); }

/* dumps memory
 *  loop_name: name of dumped loop
 *  invocation: the number of the invocation to dump
 *  count: number of arguments
 *  ...args...: the arguments to dump
 */
void dump(char *loop_name, int invocation, int count, ...) {
  /* Must be conserved ? */
  /* Avoid doing something before initializing */
  /* the dump. */
  if (!dump_initialized)
    return;

  times_called++;


  /* Happens only once */
  if ((invocation <= PAST_INV && times_called == 1) ||
      (times_called == invocation - PAST_INV)) {
    mtrace_active = true;
    send_to_tracer(TRAP_LOCK_MEM);
  }

  if (times_called != invocation) {
    return;
  }

  assert(times_called == invocation);
  mtrace_active = false;

  strcpy(tracer_buff.str_tmp, loop_name);

  /* Send args */
  send_to_tracer(TRAP_START_ARGS);
  send_to_tracer(invocation);
  send_to_tracer(count);

  /* Dump addresses */
  int i;
  va_list ap;
  va_start(ap, count);
  for (i = 0; i < count; i++) {
    send_to_tracer((register_t)va_arg(ap, void *));
  }
  va_end(ap);

  kill_after_dump = true;
  send_to_tracer(TRAP_END_ARGS);
}

void after_dump(void) {
  /* Avoid doing something before initializing */
  /* the dump. */
  if (!dump_initialized)
    return;

  if (kill_after_dump)
    abort();
}
