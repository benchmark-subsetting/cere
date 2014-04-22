#define _GNU_SOURCE
#include <dlfcn.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>
#include <malloc.h>
#include <fcntl.h>
#include <sys/mman.h>

static void* (*real_malloc)(size_t)=NULL;
static void* (*real_calloc)(size_t nmemb, size_t size)=NULL;
static void* (*real_realloc)(void *ptr, size_t size)=NULL;
static void* (*real_memalign)(size_t alignment, size_t size)=NULL;

inline static char * round_to_page(char * addr)
{
  int pagesize = sysconf(_SC_PAGE_SIZE);
  char * start_of_page = (char *)(((off64_t) addr) & ~(pagesize-1));
  return start_of_page;
}

void 
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
    int result = mprotect(round_to_page(p), size, PROT_NONE);
    assert(result != -1);
    return p;
}

void *calloc(size_t nmemb, size_t size)
{
    if(real_calloc==NULL) mtrace_init();

    void *p = NULL;
    p = real_calloc(nmemb, size);
    int result = mprotect(round_to_page(p), nmemb*size, PROT_NONE);
    assert(result != -1);
    return p;
}

void *realloc(void *ptr, size_t size)
{
    if(real_realloc==NULL) mtrace_init();

    void *p = NULL;
    p = real_realloc(ptr, size);
    int result = mprotect(round_to_page(p), size, PROT_NONE);
    assert(result != -1);
    return p;
}

void *memalign(size_t alignment, size_t size)
{
    if(real_memalign==NULL) mtrace_init();

    void *p = NULL;
    p = real_memalign(alignment, size);
    int result = mprotect(round_to_page(p), size, PROT_NONE);
    assert(result != -1);
    return p;
}
