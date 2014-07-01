#include <stdbool.h>
#include <stdio.h>
#include <signal.h>
#include "pages.h"
#include "../ccan/ccan/htable/htable.h"

/* Public Interface */
void dump_init(bool);
void dump_close(void);

void dump(char*, int, int, ...);
void after_dump(void);

bool is_mru(void * addr); 

#define MAX_DIGITS 12
#define MAX_LOG_SIZE 64
#define LOG_SIZE 64
#define MAX_STACK 64
#define MAX_PATH 256
#define MAX_IGNORE 32
#define CALLOC_INIT 512

enum dump_sa {
    MRU_SA,
    IGNORE_SA,
    DUMP_SA
};

struct dump_state {
    void* (*real_malloc)(size_t);
    void* (*real_calloc)(size_t nmemb, size_t size);
    void* (*real_realloc)(void *ptr, size_t size);
    void* (*real_memalign)(size_t alignment, size_t size);
    char * dump_prefix;
    char * pagelog_suffix;
    char * core_suffix;
    struct sigaction sa;
    bool page_log_active;
    bool kill_after_dump;
    bool global_dump;
    bool dump_initialized;
    int last_page;
    int log_size;
    int mem_fd;
    int last_ignored;
    enum dump_sa dump_sa;
    struct htable counters;
    bool mtrace_active;
    bool  mtrace_init_called;
    int stack_pos;
    int dump_active_pos;
    char hs[PAGESIZE+BUFSIZ];
    bool dump_active[MAX_STACK];
    char dump_path[MAX_STACK][MAX_PATH];
    char * pages_cache[MAX_LOG_SIZE];
    char * pages_ignored[MAX_IGNORE];
    char * calloc_init_mem[CALLOC_INIT];
    char filler __attribute__ ((aligned (PAGESIZE))) ;
} __attribute__ ((packed));

extern struct dump_state state;

