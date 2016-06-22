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
#include "err.h"
#include "ptrace.h"
#include "syscall_interface.h"

#define _DEBUG 1
/* #undef _DEBUG */

char writing_buff[1024];

static int times_called = 0;

struct dump_state state __attribute__((aligned(PAGESIZE)));
extern const char *__progname;

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
  
  state.mtrace_active = false;

  char tracer_path[] = "/home/yohan/Documents/cere/src/memory_dump/.libs/tracer";
  
  /* Copy the original binary */
  copy("/proc/self/exe", "lel_bin");

  /* configure atexit */
  atexit(dump_close);

  pid_t child = 0;
  int status = 0;

  char *const envs[] = {"LD_PRELOAD=/usr/local/lib/libcere_syscall.so",NULL};
  child = fork();

  if (child == (pid_t)-1) {
    errx(EXIT_FAILURE, "fork() failed: %s.\n", strerror(errno));
  }

  /* If we are the parent */
  if (child != 0) {
    char child_str[16];
    snprintf(child_str, sizeof(child_str), "%d", child);
    execle(tracer_path, "tracer", child_str, (char*)NULL, envs);
    errx(EXIT_FAILURE,"ERROR TRACER %s\n", strerror(errno));
    return;
  } else {
    int d = prctl(PR_SET_DUMPABLE, (long)1);
    if (d == -1) 
      errx(EXIT_FAILURE, "Prctl : %s\n", strerror(errno));

    ptrace_me();
    send_to_tracer((register_t) &state);
    send_to_tracer(sizeof(state));
    send_to_tracer((register_t)__errno_location());
    send_to_tracer((register_t) state.str_tmp);
    send_to_tracer((register_t)(&writing_buff));

    /* fprintf(stderr, "WB : %p\n", (void*)(&writing_buff)); */
    /* fprintf(stderr,"STATE : %p\n", &state);  */
    /* fprintf(stderr,"SIZE_STATE : %lu\n", sizeof(state));  */
    /* fprintf(stderr,"ERRNO_LOCATION : %p\n", __errno_location());  */
    /* fprintf(stderr,"STRING TMP : %p\n", state.str_tmp);  */
  
    /* Must be conserved ? */
    state.dump_initialized = true;
  }

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

  /* Must be conserved ? */
  //Avoid doing something before initializing
  //the dump.
  if (!state.dump_initialized)
    return;

  times_called++;

  /* fprintf(stderr, "Times called %p\n", &times_called); */

  /* Happens only once */
  if ((invocation <= PAST_INV && times_called == 1) || (times_called == invocation - PAST_INV)) {
    state.mtrace_active = false;
    sigtrap();
  }

  if (times_called != invocation) {
    state.mtrace_active = true;
    return;
  }

  assert(times_called == invocation);

  /* Send args */
  strcpy(state.str_tmp, loop_name); 
  send_to_tracer(0);
  send_to_tracer(invocation);
  send_to_tracer(count);

  /* Dump addresses */
  int i;
  va_list ap;
  va_start(ap, count);
  for (i = 0; i < count; i++) {
    send_to_tracer((register_t) va_arg(ap, void*));
  }
  va_end(ap);
  
  state.kill_after_dump = true;
  sigtrap();
}

void after_dump(void) {
  //Avoid doing something before initializing
  //the dump.
  if (!state.dump_initialized)
    return;

  if (state.kill_after_dump) {
    exit(EXIT_SUCCESS);
  }
}
