#define _LARGEFILE64_SOURCE

#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ptrace.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <sys/user.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <omp.h>
#include <err.h>
#include <features.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdbool.h>
#include <fcntl.h>
#include <stdint.h>

#include "../ccan/ccan/htable/htable.h"

#include "counters.h"
#include "syscall_interface.h"
#include "pages.h"
#include "ptrace.h"
#include "tracer.h"


#define _DEBUG 1
/* #undef _DEBUG */

int log_size = LOG_SIZE;
int last_trace = 0;
int last_page = 0;
char *pagelog_suffix = "hotpages.map";
char *core_suffix = "core.map";
char *dump_prefix = ".cere";
char *dump_root = "dumps";
char *pages_cache[LOG_SIZE];
char dump_path[MAX_PATH];
char *pages_trace[TRACE_SIZE];
void *addr_state = NULL;
size_t size_state = 0;
void* errnol = NULL;
char loop_name[SIZE_LOOP];
int invocation;
int count;
void *writing_map = NULL;
void *str_tmp_tracee = NULL;


enum tracer_state_t {
  TRACER_UNLOCKED = 1,
  TRACER_LOCKED = 2,
  TRACER_DUMPING = 3
};

enum tracer_state_t tracer_state = 0;


bool is_mru(void *addr) {
  char *start_of_page = round_to_page(addr);
  bool present = false;
  for (int i = 0; i < log_size; i++) {
    if (pages_cache[i] == start_of_page) {
      present = true;
      break;
    }
  }
  return present;
}

static void write_binary(pid_t pid, int out, void *in, int nbyte) {
  write_page(pid, out, in, nbyte);
}

static void dump_core(int count, void *addresses[]) {
  fprintf(stderr, "\nDUMP CORE\n");

  char path[MAX_PATH];
  snprintf(path, sizeof(path), "%s/%s", dump_path, core_suffix);

  FILE *core_map = fopen(path, "w");
  if (!core_map) {
    errx(EXIT_FAILURE, "Could not create core.map file : %s\n", strerror(errno));
  }

  int i;
  for (i = 0; i < count; i++) {
    fprintf(core_map, "%d %lx\n", i, (off64_t) addresses[i]);
  }
  fclose(core_map);
 
  fprintf(stderr, "END DUMP CORE\n\n");

}

static void dump_page(pid_t pid, void* start) {
  char current_path[MAX_PATH];
  snprintf(current_path, sizeof(current_path), "%s/%012llx.memdump", dump_path, (long long unsigned int)start);
  put_string(pid, current_path, str_tmp_tracee, MAX_PATH); 
  if (access(current_path, F_OK) != -1)
    return;

  int out = (int)openat_i(pid, str_tmp_tracee);
  write_binary(pid, out, start, PAGESIZE);
  close_i(pid, out);
}

static void dump_handler(int pid, void *start_of_page) {
#ifdef _DEBUG
  fprintf(stderr, "Dump handler detected access at %p\n", start_of_page);
#endif
  /* Unprotect Page */
  void * old_addr = ptrace_ripat(pid, writing_map);
  unprotect(pid, start_of_page, PAGESIZE);
  dump_page(pid, start_of_page);
  ptrace_ripat(pid, old_addr);
}

static void mru_handler(int pid, void *start_of_page) {
#ifdef _DEBUG
  fprintf(stderr, "MRU Detected access at %p\n", start_of_page);
#endif

  bool present = is_mru(start_of_page);
  assert(present == false);

  /* Unprotect Page */
  void * old_addr = ptrace_ripat(pid, writing_map);
  unprotect(pid, start_of_page, PAGESIZE);
  ptrace_ripat(pid, old_addr);

  /* Add page to page cache */
  /* we need to evict one of the pages, reprotect it ! */
  if (pages_cache[last_page] != 0) {    
    /* add the evicted page to the trace */
    pages_trace[last_trace] = pages_cache[last_page];
    last_trace = (last_trace + 1) % TRACE_SIZE;

#ifdef _DEBUG
    fprintf(stderr, "MRU Reprotecting page %p\n", pages_cache[last_page]);
#endif
    void * old_addr = ptrace_ripat(pid, writing_map);
    protect(pid, pages_cache[last_page], PAGESIZE);
    ptrace_ripat(pid, old_addr);
  }

  pages_cache[last_page] = start_of_page;
  last_page = (last_page + 1) % log_size;

/* #ifdef _DEBUG */
/*   fprintf(stderr, "     >>>> page cache\n"); */
/*   for (int i = 0; i < log_size; i++) { */
/*     int c = (i + last_page) % log_size; */
/*     fprintf(stderr, "     >>>> %p\n", pages_cache[c]); */
/*   } */
/* #endif */
}

siginfo_t wait_sigtrap(pid_t child) {
  while (true) {
    siginfo_t sig;
    sig = wait_process(child);
    show_registers(stderr, child, "wait_sigtrap");
    
    if (sig.si_signo == SIGTRAP || sig.si_signo == (SIGTRAP|0x80)) {
      return sig;
    } 
    else if (sig.si_signo == SIGSEGV) {
      void *addr = round_to_page(sig.si_addr);
      fprintf(stderr, "state = %d\n", tracer_state);
      switch(tracer_state) {
      case TRACER_UNLOCKED: 
	/* We should never get a sigsegv in unlocked state ! */
	errx(EXIT_FAILURE, "SIGSEGV at %p before locking memory during capture\n", sig.si_addr);
      case TRACER_LOCKED:
	mru_handler(child, addr);
	break;
      case TRACER_DUMPING:
	dump_handler(child, addr);
	break;
      default: assert(false); /* we should never be here */
      }

      show_registers(stderr, child, "after sigsegv");
      ptrace_cont(child);
    } 
    else {
      errx(EXIT_FAILURE, "Error after lock_mem and before dump %s\n", strerror(sig.si_signo));
    }
  }
  fprintf(stderr, "\n");
}


static register_t receive_from_tracee(pid_t child) {
  register_t ret;
  siginfo_t sig;
  sig = wait_sigtrap(child);
  assert(sig.si_signo == SIGTRAP || sig.si_signo == (SIGTRAP|0x80));
  ret = get_arg_from_regs(child);
  ptrace_cont(child);
  return ret;
}


void debug(const char msg[]) { 
  fprintf(stderr, "%s", msg);
}

void create_dump_dir(void) {
  struct stat sb;
  /* Check that dump exists or try to create it, then enter it */
  if (stat(dump_prefix, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(dump_prefix, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create %s %s", dump_prefix, strerror(errno));
  }

  char dump_root_[MAX_PATH];
  snprintf(dump_root_, sizeof(dump_root_), "%s/%s", dump_prefix, dump_root);

  if (stat(dump_root_, &sb) == -1 || (!S_ISDIR(sb.st_mode))) {
    if (mkdir(dump_root_, 0777) != 0)
      errx(EXIT_FAILURE, "Could not create %s: %s", dump_root_, strerror(errno));
  }
}

static void read_map(pid_t pid) {
  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);
  FILE *maps = fopen(maps_path, "r");
  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");
  char buf[BUFSIZ + 1];
  while (fgets(buf, BUFSIZ, maps)) {
    fprintf(stderr, "%s", buf);
  }
  fclose(maps);
}

void tracer_lock_mem(pid_t pid) {

  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);
  FILE *maps = fopen(maps_path, "r");

  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  void *addresses[65536];
  char buf[BUFSIZ + 1];
  int counter = 0;

#ifdef _DEBUG
  fprintf(stderr, "\n***\tSTART LOCK MEM\t***\n");
#endif 

  char *start_of_stack, *end_of_stack;

  while (fgets(buf, BUFSIZ, maps)) {
    void *start, *end;

    fprintf(stderr, "%s", buf);
    sscanf(buf, "%p-%p", &start, &end);
    
    if (start == writing_map)
      continue;
    
    /* Stack is special, it should be protected last
       because we are using it */
    if (strstr(buf, "stack") != NULL) {
      end_of_stack = (char *)end;
      start_of_stack = (char *)start;
      continue;
    }
    
    /* Ignore libdump mem zones */
    if (strstr(buf, "libcere_syscall.so") != NULL)
      continue;

    /* Ignore libdump mem zones */
    if (strstr(buf, "libcere_dump.so") != NULL)
      continue;

    /* Ignore libc pages */
    if (strstr(buf, "linux-gnu") != NULL)
      continue;

    /* Ignore libc special mem zones */
    if (strstr(buf, "r-xp") != NULL)
      continue;

    /* Ignore vsyscall special mem zones */
    if (strstr(buf, "vsyscall") != NULL)
      continue;

    /* Ignore libdump vdso zones */
    if (strstr(buf, "vdso") != NULL)
      continue;

    /* Ignore vvar zone (cf. https://lkml.org/lkml/2015/3/12/602)*/
    if (strstr(buf, "vvar") != NULL)
      continue;

    /* Ignore alreay protected pages */
    if (strstr(buf, "---p") != NULL)
      continue;

    void *page_start = round_to_page(start);

    if (page_start <= errnol && errnol < end) {
      /* We split the range containing errnol into two segments */

      /* If the left segment is non-empty, we must lock it */
      if (page_start < errnol) {
        assert(count < 65536);
        addresses[counter++] = page_start;
        addresses[counter++] = errnol;
      }

      /* If the right segment is non-empty, we must lock it */
      if (errnol + PAGESIZE < end) {
        assert(counter < 65536);
        addresses[counter++] = (errnol + PAGESIZE);
        addresses[counter++] = end;
      }
      continue;
    }
    
    assert(counter < 65536);
    addresses[counter++] = page_start;
    addresses[counter++] = end;
  }

  /* Protect all pages in adresses */
  while (counter > 0) {
    void *end = addresses[--counter];
    void *start = addresses[--counter];
    protect(pid, start, (end - start));
  }
  
  unprotect_state(pid, addr_state, size_state);

  /* Closing maps file must be done after locking the memory. If this close is
   * done before, it changes the memory mapping kept inside addresses making
   * the above SYS_mprotect call fail */
  int r = fclose(maps);
  if (r != 0)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ %s\n", strerror(errno));

  protect(pid, start_of_stack, (end_of_stack - start_of_stack));

  read_map(pid);

  fprintf(stderr, "***\tLOCK MEM END\t***\n\n");
}

void flush_hot_pages_trace_to_disk(pid_t pid) {
  char path[MAX_PATH];
  char buf[14];

  fprintf(stderr, "\nFLUSH HOT PAGE\n");
  
  snprintf(path, sizeof(path), "%s/%s", dump_path, pagelog_suffix);
  FILE *out = fopen(path, "w");
  if (out == 0) 
    errx(EXIT_FAILURE, "Error open %s : %s\n", path, strerror(errno));
  
  for (int i = 0; i < TRACE_SIZE; i++) {
    int c = (i + last_trace) % TRACE_SIZE;
    if (pages_trace[c] != 0) { 
      fprintf(out, "%p\n", pages_trace[c]);
    }
  }

  /* Dump the log */
  for (int i = 0; i < log_size; i++) {
    int c = (i + last_page) % log_size;
    if (pages_cache[c] != 0) {      
      fprintf(out, "%p\n", pages_cache[c]);
    }
  }

  fclose(out);
  fprintf(stderr, "END FLUSH HOT PAGE\n\n");

}

void dump_unprotected_pages(pid_t pid) {
  fprintf(stderr, "\nDUMP UNPROTECTED PAGE\n");

  /* Dump the unprotected pages before entering the codelet region. */
  for (int i = 0; i < log_size; i++) {
    int c = (i + last_page) % log_size;
    if (pages_cache[c] != 0) {
      dump_page(pid, pages_cache[c]);
    }
  }
  fprintf(stderr, "END DUMP UNPROTECTED PAGE\n\n");
}

void dump_arg(pid_t pid) {
  
  fprintf(stderr, "DUMP( %s %d count = %d) \n", loop_name, invocation, count);

  /* Ensure that the dump directory exists */
  snprintf(dump_path, sizeof(dump_path), "%s/%s/%s",
	   dump_prefix, dump_root, loop_name);

  mkdir(dump_path, 0777);

  snprintf(dump_path, sizeof(dump_path), "%s/%s/%s/%d",
	   dump_prefix, dump_root, loop_name, invocation);

  if (mkdir(dump_path, 0777) != 0)
    errx(EXIT_FAILURE, "dump %s already exists, stop\n", dump_path);


  int i;
  void* addresses[count];
  for (i = 0; i < count; i++)
    addresses[i] = (void*) receive_from_tracee(pid);
  
  // Wait for end of arguments sigtrap
  wait_sigtrap(pid);

  /*Dump hotpages to disk*/
  flush_hot_pages_trace_to_disk(pid);

  char lel_bin_path[1024];
  /*Link to the original binary*/
  snprintf(lel_bin_path, sizeof(lel_bin_path), "%s/lel_bin", dump_path);
  int res = linkat(AT_FDCWD, "lel_bin", AT_FDCWD, lel_bin_path,
		   AT_SYMLINK_FOLLOW);
  if (res == -1)
    errx(EXIT_FAILURE, "Error copying the dump binary\n");

  for (i = 0; i < count ; i++) {
    void *start_of_page = round_to_page(addresses[i]);
    if (start_of_page != NULL) {
      unprotect(pid, start_of_page, PAGESIZE);
      dump_page(pid, start_of_page);
    }
  }

  dump_core(count, addresses);
  dump_unprotected_pages(pid);
}

void read_args(pid_t pid) {
  fprintf(stderr, "HERE 2\n");
  wait_sigtrap(pid);
  ptrace_getdata(pid, (long long unsigned)str_tmp_tracee, loop_name, SIZE_LOOP);
  ptrace_cont(pid);
  invocation = (int)receive_from_tracee(pid);
  count = (int)receive_from_tracee(pid);

#ifdef _DEBUG
  fprintf(stderr,"LOOP NAME : %s\n",loop_name); 
  fprintf(stderr,"INVOCATION : %d\n",invocation); 
  fprintf(stderr,"COUNT : %d\n",count); 
#endif

}

void tracer_dump(pid_t pid) {
  read_args(pid);
  dump_arg(pid);
}

void tracer_init(pid_t pid) {

  ptrace_attach(pid);
  create_dump_dir();

  addr_state = (void*)receive_from_tracee(pid);
  size_state = (size_t)receive_from_tracee(pid);
  errnol = round_to_page((void*)receive_from_tracee(pid));
  str_tmp_tracee = (void*)receive_from_tracee(pid);

  writing_map = (void*)receive_from_tracee(pid);
  fprintf(stderr, "WM : %p\n", writing_map);

#ifdef _DEBUG
  /* fprintf(stderr,"STATE : %p\n", addr_state);  */
  /* fprintf(stderr,"SIZE_STATE : %lu\n", size_state);  */
  /* fprintf(stderr,"ERRNO_LOCATION : %p\n", errnol);  */
  fprintf(stderr,"STRING TMP : %p\n", str_tmp_tracee);

  fprintf(stderr, "***\tTRACER INIT DONE\t***\n\n");
#endif
}


int main(int argc, char *argv[]) {

  pid_t child = 0;

  child = atoi(argv[1]);

  tracer_init(child);
  fprintf(stderr, "******* TRACER_UNLOCKED\n");
  tracer_state = TRACER_UNLOCKED;

  // Wait for lock_mem trap
  wait_sigtrap(child);
  fprintf(stderr, "******* Start Locking\n");
  unprotect(child, round_to_page(writing_map), PAGESIZE);
  tracer_lock_mem(child);

  fprintf(stderr, "******* TRACER_LOCKED\n");
  tracer_state = TRACER_LOCKED;
  ptrace_cont(child);


  // Dump arguments 
  tracer_dump(child);

  fprintf(stderr, "******* TRACER_DUMPING */\n");
  tracer_state = TRACER_DUMPING;
  ptrace_cont(child);

  while(1) {
    wait_sigtrap(child);
  }

  ptrace_detach(child);

  return EXIT_SUCCESS;
}

 
