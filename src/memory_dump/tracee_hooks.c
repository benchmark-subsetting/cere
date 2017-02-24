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
#define _GNU_SOURCE
#include <assert.h>
#include <dlfcn.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>

#include "tracee.h"
#include "pages.h"
#include "tracee_interface.h"
#include "types.h"

static void *(*real_malloc)(size_t) = NULL;
static void *(*real_calloc)(size_t nmemb, size_t size) = NULL;
static void *(*real_realloc)(void *ptr, size_t size) = NULL;
static void *(*real_memalign)(size_t alignment, size_t size) = NULL;
static FILE *(*real_fopen)(const char *path, const char *mode) = NULL;
static size_t (*real_fread)(void *ptr, size_t size, size_t nmemb, FILE *stream);
static size_t (*real_fwrite)(const void *ptr, size_t size, size_t nmemb,
                             FILE *stream);

static char *calloc_init_mem[CALLOC_INIT];
static bool mtrace_init_called = false;

/* lock_range: requests protection of the memory range [from; to] */
static void lock_range(void *from, void *to) {
  if (mtrace_active) {
    hook_sigtrap();
    send_to_tracer((register_t)from);
    send_to_tracer((register_t)to);
  }
}

/* hooks_init: replace memory allocation functions by our own hooks that
 * protect memory before returning it to the user */
static void hooks_init(void) {

  mtrace_init_called = true;

  real_malloc = dlsym(RTLD_NEXT, "malloc");
  if (NULL == real_malloc) {
    fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
  }
  real_calloc = dlsym(RTLD_NEXT, "calloc");
  if (NULL == real_malloc) {
    fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
  }
  real_realloc = dlsym(RTLD_NEXT, "realloc");
  if (NULL == real_malloc) {
    fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
  }
  real_memalign = dlsym(RTLD_NEXT, "memalign");
  if (NULL == real_malloc) {
    fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
  }
  real_fopen = dlsym(RTLD_NEXT, "fopen");
  if (NULL == real_malloc) {
    fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
  }
  real_fread = dlsym(RTLD_NEXT, "fread");
  if (NULL == real_malloc) {
    fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
  }
  real_fwrite = dlsym(RTLD_NEXT, "fwrite");
  if (NULL == real_malloc) {
    fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
  }
}

void *malloc(size_t size) {
  if (real_malloc == NULL)
    hooks_init();

  void *p = NULL;
  p = real_malloc(size);
  lock_range(p, p + size);
  return p;
}

void *calloc(size_t nmemb, size_t size) {

  /* On certain versions of linux and when linking with pthreads, */
  /* dlsym uses calloc. This can cause an infinite loop when calling */
  /* hooks_init(). To avoid this, we return a statically allocated block of */
  /* memory in that case. Reported by Sebastien Valat. */

  if (real_calloc == NULL && mtrace_init_called) {
    assert(size < CALLOC_INIT);
    assert(nmemb * size < CALLOC_INIT);
    /* return state.calloc_init_mem; */
    return calloc_init_mem;
  }

  if (real_calloc == NULL)
    hooks_init();

  void *p = NULL;
  p = real_calloc(nmemb, size);
  lock_range(p, p + size);
  return p;
}

void *realloc(void *ptr, size_t size) {
  if (real_realloc == NULL)
    hooks_init();

  void *p = NULL;
  p = real_realloc(ptr, size);
  lock_range(p, p + size);
  return p;
}

void *memalign(size_t alignment, size_t size) {
  if (real_memalign == NULL)
    hooks_init();

  void *p = NULL;
  p = real_memalign(alignment, size);
  lock_range(p, p + size);
  return p;
}

/* touch_string: touch all the memory in a char* string */
static void touch_string(const char *str) {
  const char *c;
  for (c = str; *c != '\0'; c++)
    ;
}

/* touch_mem: touches all the pages in the memory range [mem; mem + size*nmemb]
 */
static void touch_mem(const void *mem, size_t size, size_t nmemb) {
  size_t i;
  char *last = ((char *)mem) + size * nmemb - 1;
  for (char *c = (char *)mem; c <= last; c += PAGESIZE) {
    volatile char touched = *c;
  }
  volatile char touched = *last;
}

/* Before performing standard syscall, we must unlock memory before calling the
 * kernel */
FILE *fopen(const char *path, const char *mode) {
  if (real_fopen == NULL)
    hooks_init();
  touch_string(path);
  touch_string(mode);
  return real_fopen(path, mode);
}

size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream) {
  if (real_fread == NULL)
    hooks_init();
  touch_mem(ptr, size, nmemb);
  return real_fread(ptr, size, nmemb, stream);
}

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
  if (real_fwrite == NULL)
    hooks_init();
  touch_mem(ptr, size, nmemb);
  return real_fwrite(ptr, size, nmemb, stream);
}
