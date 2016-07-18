/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2016, Universite de Versailles St-Quentin-en-Yvelines  *
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
#include "dump.h"
#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <unistd.h>

#include "err.h"
#include "pages.h"
#include "ptrace.h"
#include "syscall_interface.h"
#include "types.h"

#define _DEBUG 1
#undef _DEBUG

#include "debug.h"

static int times_called = 0;
static bool dump_initialized;
static bool kill_after_dump;

void *(*real_malloc)(size_t);
void *(*real_calloc)(size_t nmemb, size_t size);
void *(*real_realloc)(void *ptr, size_t size);
void *(*real_memalign)(size_t alignment, size_t size);
bool mtrace_active;
char *calloc_init_mem[CALLOC_INIT];
char writing_buff[PAGESIZE] __attribute__((aligned(PAGESIZE)));

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

void dump_init(void) {

  /* state.mtrace_active = false; */
  mtrace_active = false;

  /* Copy the original binary */
  copy("/proc/self/exe", "lel_bin");

  /* configure atexit */
  atexit(dump_close);

  pid_t child = 0;

  child = fork();

  if (child == (pid_t)-1) {
    errx(EXIT_FAILURE, "fork() failed: %s.\n", strerror(errno));
  }

  /* If we are the parent */
  if (child != 0) {
    char child_str[16];
    snprintf(child_str, sizeof(child_str), "%d", child);
    execlp("cere-tracer", "cere-tracer", child_str, (char *)NULL);
    errx(EXIT_FAILURE, "ERROR TRACER RUNNING : %s\n", strerror(errno));
    return;

  } else {

    int d = prctl(PR_SET_DUMPABLE, (long)1);
    if (d == -1)
      errx(EXIT_FAILURE, "Prctl : %s\n", strerror(errno));

    ptrace_me();
    raise(SIGSTOP);

    /* Send memeory map address use to inject code */
    send_to_tracer((register_t)(&writing_buff));

    /* Must be conserved ? */
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
    sigtrap();
  }

  if (times_called != invocation) {
    return;
  }

  assert(times_called == invocation);
  mtrace_active = false;

  /* Send args */
  strcpy(writing_buff + OFFSET_STR, loop_name);
  send_to_tracer(0);
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
  sigtrap();
}

void after_dump(void) {
  /* Avoid doing something before initializing */
  /* the dump. */
  if (!dump_initialized)
    return;

  if (kill_after_dump)
    exit(EXIT_SUCCESS);
}
