#define _GNU_SOURCE
#include <dlfcn.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>
#include <malloc.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdbool.h>
#include "pages.h"
#include "dump.h"

static void* (*real_malloc)(size_t)=NULL;
static void* (*real_calloc)(size_t nmemb, size_t size)=NULL;
static void* (*real_realloc)(void *ptr, size_t size)=NULL;
static void* (*real_memalign)(size_t alignment, size_t size)=NULL;

void mtrace_activate(void)
{
  state.mtrace_active = true;
}

void mtrace_deactivate(void)
{
  state.mtrace_active = false;
}

static void 
mtrace_init(void)
{
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
}

void*
malloc(size_t size)
{
  if(real_malloc==NULL) mtrace_init();

  void *p = NULL;
  p = real_malloc(size);
  if (state.mtrace_active && !is_mru(p)) {
      int result = mprotect(round_to_page(p), round_up_page(size), PROT_NONE);
      assert(result != -1);
  }
  return p;
}

void *calloc(size_t nmemb, size_t size)
{
  if(real_calloc==NULL) mtrace_init();

  void *p = NULL;
  p = real_calloc(nmemb, size);
  if (state.mtrace_active && !is_mru(p)) {
      int result = mprotect(round_to_page(p), round_up_page(nmemb*size), PROT_NONE);
      assert(result != -1);
  }
  return p;
}

void *realloc(void *ptr, size_t size)
{
  if(real_realloc==NULL) mtrace_init();

  void *p = NULL;
  p = real_realloc(ptr, size);
  if (state.mtrace_active && !is_mru(p)) {
      int result = mprotect(round_to_page(p), round_up_page(size), PROT_NONE);
      assert(result != -1);
  }
  return p;
}

void *memalign(size_t alignment, size_t size)
{
  if(real_memalign==NULL) mtrace_init();

  void *p = NULL;
  p = real_memalign(alignment, size);
  if (state.mtrace_active && !is_mru(p)) {
      int result = mprotect(round_to_page(p), round_up_page(size), PROT_NONE);
      assert(result != -1);
  }
  return p;
}
