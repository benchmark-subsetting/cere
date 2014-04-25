#include <stdbool.h>
#include <stdio.h>
#include <signal.h>
#include "pages.h"
#include "../htable/htable/htable.h"

/* Public Interface */
void dump_init(void);
void dump_close(void);

void dump(char*, int, int, ...);
void after_dump(void);

#define MAX_LOG_SIZE 1024
#define LOG_SIZE 1024
#define MAX_STACK 64
#define MAX_PATH 256
#define MAX_DIGITS 12

enum dump_sa {
    MRU_SA,
    IGNORE_SA,
    DUMP_SA
};

struct dump_state {
    struct sigaction sa;
    bool page_log_active;
    int last_page;
    int log_size;
    int mem_fd;
    enum dump_sa dump_sa;
    struct htable counters;
    bool mtrace_active;
    int stack_pos;
    char hs[PAGESIZE+BUFSIZ];
    char dump_path[MAX_STACK][MAX_PATH];
    char * pages_cache[MAX_LOG_SIZE];
    char filler __attribute__ ((aligned (PAGESIZE))) ;
} __attribute__ ((packed));

extern struct dump_state state;

