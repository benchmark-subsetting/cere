#define _LARGEFILE64_SOURCE
#include <assert.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdbool.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "snoop.h"

#define PAGESIZE 4096

#define handle_error(msg) \
  do { perror(msg); exit(EXIT_FAILURE); } while (0)

struct page_cache {
    struct sigaction sa1;
    bool page_log_active;
    int last_page __attribute__ ((aligned (PAGESIZE)));
    int log_size;
    char * pages_cache[256];
} __attribute__ ((packed));


char hs[PAGESIZE] __attribute__ ((aligned (PAGESIZE)));

struct page_cache pc;

extern void mtrace_init(void);

#define round_to_page(addr) ((char *)(((off64_t)(addr)) & ~(PAGESIZE-1))) 

static void
install_handler(int sig, siginfo_t *si, void *unused)
{
  char * touched_addr = si->si_addr;
  char * start_of_page = round_to_page(touched_addr);

  /* Unprotect Page */
  int result = mprotect((void*)start_of_page, PAGESIZE, PROT_READ|PROT_WRITE);
  assert(result != -1);
}


static void
log_handler(int sig, siginfo_t *si, void *unused)
{
  char * touched_addr = si->si_addr;
  char * start_of_page = round_to_page(touched_addr);

#ifdef _DEBUG
  printf("Detected access at: %p -- Start of Page: %p\n",
         touched_addr, start_of_page);
#endif


  /* Unprotect Page */
  int result = mprotect((void*)start_of_page, PAGESIZE, PROT_READ|PROT_WRITE|PROT_EXEC);
  assert(result != -1);

  /* Add page to page cache */
  
  /* we need to evict one of the pages, reprotect it ! */
  if (pc.pages_cache[pc.last_page] != 0) {
#ifdef _DEBUG
      printf("Reprotecting page %p\n", pc.pages_cache[pc.last_page]);
#endif
      int result = mprotect((void*)pc.pages_cache[pc.last_page], PAGESIZE, PROT_EXEC);
      assert(result != -1);
  } 

  pc.pages_cache[pc.last_page] = start_of_page;
  pc.last_page = (pc.last_page + 1)%pc.log_size;

#ifdef _DEBUG
  printf("page cache [ ");
  for (int i = 0; i <pc.log_size; i++) {
      int c = (i + pc.last_page) % pc.log_size;
      printf("%p ", pc.pages_cache[c]);
  }
  printf(" ]\n");
#endif
}

void
page_log_on(int log_size)
{
  assert(pc.page_log_active == false);

  mtrace_init();


  char *p;
  pc.last_page = 0;
  pc.log_size = log_size;

  char path[BUFSIZ];

  /* Configure an alternative stack for 
     the signal handler */
  stack_t ss;
  ss.ss_sp = hs;
  ss.ss_size = sizeof(hs);
  ss.ss_flags = 0;
  if (sigaltstack(&ss, NULL) == -1)
    handle_error("sigaltstack");

  pc.sa1.sa_flags = SA_SIGINFO | SA_ONSTACK;
  sigemptyset(&pc.sa1.sa_mask);
  pc.sa1.sa_sigaction = install_handler;

  if (sigaction(SIGSEGV, &pc.sa1, NULL) == -1)
    handle_error("sigaction");

  pid_t my_pid = getpid();
  snprintf(path, sizeof(path), "/proc/%d/maps", my_pid);
  FILE * maps = fopen(path, "r");

  if(!maps) {
      fprintf(stderr, "Error reading the memory using /proc/ interface");
      exit(-1);
  }

  char buf[BUFSIZ + 1];
  size_t start, end;
  int counter = 0;
  char *start_of_stack, *end_of_stack;

  char* addresses[1000];
  int count = 0;

  while(fgets(buf, BUFSIZ, maps)) {
      off64_t start, end;

      sscanf(buf, "%lx-%lx", &start, &end);

      /* Stack is special, it should be protected last
         because we are using it */
      if (strstr(buf, "stack") != NULL) {
         end_of_stack = (char *) end; 
         start_of_stack = (char *) start; 
         continue; 
      }
      /* Ignore libc pages */
      if (strstr(buf, "linux-gnu") != NULL)
         continue; 
      /* Ignore libc special mem zones */
      if (strstr(buf, "r-xp") != NULL) 
         continue;
      /* Ignore vsyscall special mem zones */
      if (strstr(buf, "vsyscall") != NULL) 
         continue;

      /* Ignore prog and system segments */
      if (start < 0x500000 | start >= 0x7ffff0000000) continue;

#ifdef _DEBUG
      printf("protecting region %p-%p\n", (char*)start, (char*)end);
#endif
      char * page_start = round_to_page((char*)start);
      assert(count < 4094);
      addresses[count++] = (char*) page_start;
      addresses[count++] = (char*) end;
  }
  fclose(maps);


  while(count > 0) {
      char * end = addresses[--count];
      char * start = addresses[--count];
      int result = mprotect(start, end-start, PROT_EXEC);
      //assert(result != -1);
  }

  /* Unprotect page_cache */
  int result = mprotect(&pc, sizeof(pc), PROT_READ | PROT_WRITE);
  assert(result != -1);

  /* Unprotect handler stacl */
  result = mprotect(hs, sizeof(hs), PROT_READ | PROT_WRITE | PROT_EXEC);
  assert(result != -1);

#ifdef _DEBUG
  printf("protection activated\n");
#endif
  pc.sa1.sa_sigaction = log_handler;
  if (sigaction(SIGSEGV, &pc.sa1, NULL) == -1)
    handle_error("sigaction");

  // Now protect the stack, and pray !
#ifdef _DEBUG
  printf("protecting stack %p-%p\n", start_of_stack, end_of_stack);
#endif
  mprotect(start_of_stack, end_of_stack-start_of_stack, PROT_EXEC);

  pc.page_log_active = true;
}

void
page_log_off(void)
{
  assert(pc.page_log_active == true);
  pc.sa1.sa_sigaction = install_handler;
  if (sigaction(SIGSEGV, &pc.sa1, NULL) == -1)
    handle_error("sigaction");
  pc.page_log_active = false;
}

void
page_log_dump(char * filename)
{
  assert(pc.page_log_active == false);
  FILE * f = fopen(filename, "w");
  for (int i = 0; i <pc.log_size; i++) {
      int c = (i + pc.last_page) % pc.log_size;
      fprintf(f, "%lx\n", (off64_t)pc.pages_cache[c]);
  }
  fclose(f);
}
