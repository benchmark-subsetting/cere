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

#define BUG 0
#if BUG
static int times_called = 0;
#endif
static bool dump_initialized;
volatile static bool kill_after_dump = false;

void *(*real_malloc)(size_t);
void *(*real_calloc)(size_t nmemb, size_t size);
void *(*real_realloc)(void *ptr, size_t size);
void *(*real_memalign)(size_t alignment, size_t size);
bool mtrace_active;
long PAGESIZE;

typedef struct {
  char* loop_name;
  unsigned long long count_value;
} LoopCounter;

typedef struct {
  LoopCounter* counters;
  int num_counters;
} LoopCounterCollection;

static LoopCounterCollection loop_counters;
//static bool in_progress = false;
static char* in_progress_loop_name = NULL;
static int in_progress_invocation = -1;

static LoopCounter* get_counter_internal(const char* loop_name) {
  for (int i = 0; i < loop_counters.num_counters; i++) { 
    LoopCounter* counter = &(loop_counters.counters[i]);
    if (strcmp(counter->loop_name, loop_name) == 0) { 
      return counter;
    }
  }
  // New counter 
  LoopCounter newCounter;
  newCounter.loop_name = strdup(loop_name);
  newCounter.count_value = 0;  

  loop_counters.counters = realloc(
    loop_counters.counters, (loop_counters.num_counters + 1) * sizeof(LoopCounter));
  loop_counters.counters[loop_counters.num_counters] = newCounter;
  loop_counters.num_counters++;
  return &(loop_counters.counters[loop_counters.num_counters-1]);
}

void increment_counter(const char* loop_name) { 
  get_counter_internal(loop_name)->count_value ++;
}

unsigned long long get_counter_value(const char* loop_name) {
  return get_counter_internal(loop_name)->count_value ;
}

void _dump_preinit(bool new_kad) {
  PAGESIZE = sysconf(_SC_PAGESIZE);

  /* Set global kill_after_dump */
  kill_after_dump = new_kad;
}

// Parametrized dump_init that can be used to set kill_after_dump
// (this will enable or disable multi-codelet capture)
void _dump_init() {
  /* state.mtrace_active = false; */
  mtrace_active = false;

  /* Copy the original binary */
  char buf[BUFSIZ];
  snprintf(buf, sizeof buf, "cp /proc/%d/exe lel_bin", getpid());
  int ret = system(buf);
  if (ret != 0) {
    errx(EXIT_FAILURE, "lel_bin copy failed(%d, %d): %s.\n", getpid(), getppid(), strerror(errno));
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

    debug_print("[tracee->tracer %d] New tracer process\n", getpid());
    char args[2][32];
    snprintf(args[0], sizeof(args[0]), "%d", child);
    snprintf(args[1], sizeof(args[1]), "%p", &tracer_buff);

    char *const arg[] = {"cere-tracer", args[0], args[1], NULL};
    execvp("cere-tracer", arg);
    errx(EXIT_FAILURE, "ERROR TRACER RUNNING : %s\n", strerror(errno));
  } else {

    debug_print("[tracee->tracee %d] New tracee process\n", getpid());
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

    raise(SIGSTOP);

    dump_initialized = true;
  }

}

// Default dump_init, where kill_after_dump=true
void dump_init(void) {
  printf("DUMP_INIT %d, %d\n", getpid(), getppid());
  _dump_init();
}

// Multi dump_init, by setting kill_after_dump=false
void multi_dump_init(void) {
  debug_print("MULTI DUMP_INIT %d, %d\n", getpid(), getppid());
  _dump_init();
  //_dump_init(true);
}

// Default dump_init, where kill_after_dump=true
void dump_preinit(void) {
  _dump_preinit(true);
}

// Multi dump_init, by setting kill_after_dump=false
void multi_dump_preinit(void) {
  _dump_preinit(false);
  //_dump_init(true);
}

// We should only reach this during multi codelet capture
void dump_close() {
  debug_print("[tracee %d] Aborting\n", getpid());
  unlink("lel_bin");
  abort();
}

/* dumps memory
 *  loop_name: name of dumped loop
 *  invocation: the number of the invocation to dump
 *  count: number of arguments
 *  ...args...: the arguments to dump
 */
void vdump_orig(char *loop_name, int invocation, int count, va_list ap) {
  /* Must be conserved ? */
  /* Avoid doing something before initializing */
  /* the dump. */
  if (!dump_initialized)
    return;

	#if BUG
  times_called++;
  printf("times_called:%d, inv:%d, lm:%s\n", times_called, invocation, loop_name);
  #endif


  /* Happens only once */
  debug_print("[tracee %d] Sending trap lock mem %d, %d\n", getpid(), PAST_INV, times_called);
  mtrace_active = true;
  send_to_tracer(TRAP_LOCK_MEM);

	#if BUG
  if (times_called != invocation) {
    return;
  }

  assert(times_called == invocation);
  #endif
  mtrace_active = false;

  strcpy(tracer_buff.str_tmp, loop_name);

  /* Send args */
  send_to_tracer(TRAP_START_ARGS);
  send_to_tracer(invocation);
  send_to_tracer(count);

  /* Dump addresses */
  int i;
  //va_list ap;
  //va_start(ap, count);
  for (i = 0; i < count; i++) {
    send_to_tracer((register_t)va_arg(ap, void *));
  }
  //va_end(ap);

  send_to_tracer(TRAP_END_ARGS);
}

void dump_orig(char *loop_name, int invocation, int count, ...) {
  va_list ap;
  va_start(ap, count);
  vdump_orig(loop_name, invocation, count, ap);
  va_end(ap);
}

void after_dump_orig(void) {
  debug_print("[tracee %d] In after_dump\n", getpid());
  /* Avoid doing something before initializing */
  /* the dump. */
  if (!dump_initialized)
    return;

  pid_t parent = getppid();

  if (kill_after_dump) {
    debug_print("[tracee %d] Kill after dump, sending kill to parent\n", getpid());

    // Send a fake SIGKILL to "trick" the tracer into terminating itself
    kill(parent, 9);

    unlink("lel_bin");
    abort();
  }
  else {
    debug_print("[tracee %d] Sending abort signal to parent %d\n", getpid(), parent);

    // Send a fake SIGABRT to "trick" the tracer into freeing us & terminating itself
    kill(parent, 6);
    // Detach ourselves from our parent (=tracer)
    //setsid();
    debug_print("[tracee %d] Resume execution normally\n", getpid());
  }
}

// only invcation will be dumped and internally a counter is associated with loop
void dump(char *loop_name, int invocation, int count, ...) {
  #if 1
  //assert(!in_progress);  // should not try to call dump again while dump in progress.
  increment_counter(loop_name);
  if (in_progress_loop_name != NULL) {
    // no need to check invocation even
    return;
  }
	va_list args;
  va_start(args, count);

	unsigned long long counter_value = get_counter_value(loop_name);
  if (counter_value == invocation + 1) {
    debug_print("DUMPING for %s, %d, cnt=%lld\n", loop_name, invocation, counter_value);
    multi_dump_init();
    vdump_orig(loop_name, invocation, count, args);
    in_progress_loop_name = strdup(loop_name);
    in_progress_invocation = invocation;
  } else {
    debug_print("NOT DUMPING for %s, %d, cnt=%lld\n", loop_name, invocation, counter_value);
  }
  #else
  multi_dump_init();
  vdump_orig(loop_name, invocation, count, args);
  #endif
  va_end(args);
}

void after_dump(char* loop_name, int invocation) {
  //after_dump_orig();
  //return;
  if (in_progress_loop_name != NULL 
    && strcmp(in_progress_loop_name, loop_name) == 0 && in_progress_invocation == invocation) {
    after_dump_orig();
    free(in_progress_loop_name);
    in_progress_loop_name = NULL;
    in_progress_invocation = -1;
    debug_print("AFTER_DUMP\n");
  } else {
    debug_print("NOT AFTER_DUMP\n");
  }
  // not in progress can happen when invocation not matching counters
}