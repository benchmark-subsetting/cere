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

static bool dump_initialized;
static bool in_codelet = false;
volatile static bool kill_after_dump = false;

void *(*real_malloc)(size_t);
void *(*real_calloc)(size_t nmemb, size_t size);
void *(*real_realloc)(void *ptr, size_t size);
void *(*real_memalign)(size_t alignment, size_t size);
bool mtrace_active;
long PAGESIZE;


/**** PER-CODELET INVOCS COUNTER STRUCT ***/
// Original implementation by David C. Wong

typedef struct {
  char* codelet_name;
  unsigned long long count_value;
} InvocsCounter;

typedef struct {
  InvocsCounter* counters;
  int num_counters;
} InvocsCounterCollection;

static InvocsCounterCollection invocs_counters;
static InvocsCounterCollection after_dump_invocs_counters;

static char* in_progress_codelet_name = NULL;
static int in_progress_invocation = -1;

static InvocsCounter* get_counter_internal(const char* codelet_name) {
  for (int i = 0; i < invocs_counters.num_counters; i++) {
    InvocsCounter* counter = &(invocs_counters.counters[i]);
    if (strcmp(counter->codelet_name, codelet_name) == 0) {
      return counter;
    }
  }
  // New counter
  InvocsCounter newCounter;
  newCounter.codelet_name = strdup(codelet_name);
  newCounter.count_value = 0;

  invocs_counters.counters = realloc(
    invocs_counters.counters, (invocs_counters.num_counters + 1) * sizeof(InvocsCounter)
  );
  invocs_counters.counters[invocs_counters.num_counters] = newCounter;
  invocs_counters.num_counters++;
  return &(invocs_counters.counters[invocs_counters.num_counters-1]);
}

static InvocsCounter* get_after_dump_counter_internal(const char* codelet_name) {
  for (int i = 0; i < after_dump_invocs_counters.num_counters; i++) {
    InvocsCounter* counter = &(after_dump_invocs_counters.counters[i]);
    if (strcmp(counter->codelet_name, codelet_name) == 0) {
      return counter;
    }
  }
  // New counter
  InvocsCounter newCounter;
  newCounter.codelet_name = strdup(codelet_name);
  newCounter.count_value = 0;

  after_dump_invocs_counters.counters = realloc(
    after_dump_invocs_counters.counters, (after_dump_invocs_counters.num_counters + 1) * sizeof(InvocsCounter)
  );
  after_dump_invocs_counters.counters[after_dump_invocs_counters.num_counters] = newCounter;
  after_dump_invocs_counters.num_counters++;
  return &(after_dump_invocs_counters.counters[after_dump_invocs_counters.num_counters-1]);
}


void increment_counter(const char* codelet_name) {
  get_counter_internal(codelet_name)->count_value ++;
}

unsigned long long get_counter_value(const char* codelet_name) {
  return get_counter_internal(codelet_name)->count_value;
}

void increment_after_dump_counter(const char* codelet_name) {
  get_after_dump_counter_internal(codelet_name)->count_value ++;
}

unsigned long long get_after_dump_counter_value(const char* codelet_name) {
  return get_after_dump_counter_internal(codelet_name)->count_value;
}


/*************************/


// Fork the current process :
// - parent becomes tracer
// - child becomes tracee
// This should be called in dump_init, and in after_dump in
// the case of multi capture
void fork_tracee() {
  pid_t child = 0;
  child = fork();

  if (child == (pid_t)-1) {
    errx(EXIT_FAILURE, "fork() failed: %s.\n", strerror(errno));
  }

  /* If we are the parent */
  if (child != 0) {

    debug_print("[tracee->tracer %d] New tracer process\n", getpid());
    char args[3][32];


    snprintf(args[0], sizeof(args[0]), "%d", child);
    snprintf(args[1], sizeof(args[1]), "%p", &tracer_buff);
    if(kill_after_dump)
      snprintf(args[2], sizeof(args[2]), "%s","single");
    else
      snprintf(args[2], sizeof(args[2]), "%s", "multi");

    char *const arg[] = {"cere-tracer", args[0], args[1], args[2], NULL};
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

// Parametrized dump_init that can be used to set kill_after_dump
// (this will enable or disable multi-codelet capture)
void _dump_init(bool new_kad) {
  PAGESIZE = sysconf(_SC_PAGESIZE);

  /* Set global kill_after_dump */
  kill_after_dump = new_kad;

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

  // We always need a tracer to record the hotpages map
  fork_tracee();
}

// Default dump_init, where kill_after_dump=true
void dump_init(void) {
  _dump_init(true);
}

// Multi dump_init, by setting kill_after_dump=false
void multi_dump_init(void) {
  _dump_init(false);
}

// We should only reach this during multi codelet capture
void dump_close() {
  debug_print("[tracee %d] Aborting\n", getpid());
  unlink("lel_bin");
  abort();
}


/* Dump utility funcs */

/* Used to split the different regions/invocations from args */
int split (char* str, int ** tokens) {
  size_t strsize = strlen(str);

  // Parse string
  char * strbak = malloc((strsize+1)*sizeof(char));
  char * token;
  strcpy(strbak, str);
  int ntokens = 0;

  // Count string tokens
  token = strtok(strbak, ",");
  while( token != NULL ) {
    ntokens++;
    token = strtok(NULL, strbak);
  }
  strcpy(strbak, str);

  // Convert string tokens to int
  if(*tokens != NULL) {
    free(*tokens);
  }
  *tokens = malloc(ntokens*sizeof(int));

  strcpy(strbak, str);

  int i=0;
  token = strtok(strbak, ",");
  (*tokens)[i] = strtol(token, NULL, 10);
  while( token != NULL ) {
    (*tokens)[i] = strtol(token, NULL, 10);
    i++;
    token = strtok(NULL, strbak);
  }

  free(strbak);

  return ntokens;
}


bool find_invocation(char *invocations_str, int times_called) {
  int * invocations = NULL;
  int n_invocations = split(invocations_str, &invocations);

  int i;
  for(i=0; i<n_invocations; i++) {
    if (times_called == invocations[i]) {
      free(invocations);
      return true;
    }
  }
  free(invocations);
  return false;
}


/* dump: requests capture of a outlined region of interest. Must be called
 * before any other code in the function to be captured.
 *   - loop_name is the name of the region of interest
 *   - n_invocations is the the size of the invocations array
 *   - invocations are the target invocations that must be captured
 *   - arg_count is the number of arguments passed to the outlined function
 *   - ... are the arguments passed to the outlined function
 */
void dump(char *loop_name, char *invocations_str, int arg_count, ...) {
  /* Avoid doing something before initializing */
  /* the dump. */
  if (!dump_initialized)
    return;

  increment_counter(loop_name);

  if (in_progress_codelet_name != NULL)
    return;


  int times_called = get_counter_value(loop_name);
  bool to_capture = find_invocation(invocations_str, times_called);
  if(!to_capture)
    return;

  // This information is useful to match dump and after_dump calls
  // (in case of recursive codelets)
  in_progress_codelet_name = strdup(loop_name);
  in_progress_invocation = times_called;

  mtrace_active = true;
  send_to_tracer(TRAP_LOCK_MEM);

  in_codelet = true;
  mtrace_active = false;
  strcpy(tracer_buff.str_tmp, loop_name);

  /* Send args */
  send_to_tracer(TRAP_START_ARGS);
  send_to_tracer(in_progress_invocation);
  send_to_tracer(arg_count);

  /* Dump addresses */
  va_list ap;
  va_start(ap, arg_count);
  for (int i = 0; i < arg_count; i++) {
    send_to_tracer((register_t)va_arg(ap, void *));
  }
  va_end(ap);

  send_to_tracer(TRAP_END_ARGS);
}

void after_dump(char *loop_name) {
  debug_print("[tracee %d] In after_dump\n", getpid());
  /* Avoid doing something before initializing */
  /* the dump. */
  if (!dump_initialized || !in_codelet)
    return;

  // Increment number of after_dump calls for the codelet
  // (useful to match call & after_dump call for recursive codelets)
  increment_after_dump_counter(loop_name);

  // Check if we're stopping the correct codelet
  if (in_progress_codelet_name != NULL && strcmp(in_progress_codelet_name, loop_name) == 0)
    return;

  // Check if invoc counters of dump and after_dump match for this codelet.
  // If so, stop dump
  int n_ad_calls = get_after_dump_counter_value(loop_name);
  int n_calls = get_counter_value(loop_name);
  if(n_ad_calls != n_calls)
    return;

  in_codelet = false;
  free(in_progress_codelet_name);
  in_progress_codelet_name = NULL;
  in_progress_invocation = -1;


  if (kill_after_dump) {
    debug_print("[tracee %d] Single capture : terminating tracer and tracee\n", getpid());
    abort();
  }
  else {
    pid_t parent = getppid();
    debug_print("[tracee %d] Multi capture : spawn new tracer and resume exec\n", getpid());
    // Send a fake SIGABRT so the tracer frees us & terminates itself
    kill(parent, 6);
    // Directly spawn a new tracer to record hotpages map
    fork_tracee();
  }

}
