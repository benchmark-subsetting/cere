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

#include "types.h"
#include "pages.h"
#include "ptrace.h"
#include "syscall_interface.h"
#include "tracer.h"

#define _DEBUG 1
#undef _DEBUG

#include "debug.h"

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

enum tracer_state_t tracer_state = 0;

static void tracer_lock_range(pid_t child);

static void read_map(pid_t pid) {
  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);

  FILE *maps = fopen(maps_path, "r");
  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  char buf[BUFSIZ + 1];
  while (fgets(buf, BUFSIZ, maps))
    fprintf(stderr, "%s", buf);

  fclose(maps);
}

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
  debug_print("%s","DUMP CORE\n");

  char path[MAX_PATH];
  snprintf(path, sizeof(path), "%s/%s", dump_path, core_suffix);

  FILE *core_map = fopen(path, "w");
  if (!core_map)
    errx(EXIT_FAILURE, "Could not create core.map file : %s\n", strerror(errno));

  int i;
  for (i = 0; i < count; i++)
    fprintf(core_map, "%d %lx\n", i, (register_t) addresses[i]);
  fclose(core_map);
 
  debug_print("%s", "END DUMP CORE\n");
}

static void dump_page(pid_t pid, void* start) {
  char current_path[MAX_PATH];
  snprintf(current_path, sizeof(current_path), "%s/%012lx.memdump", dump_path, (register_t)start);
  put_string(pid, current_path, str_tmp_tracee, MAX_PATH); 
  if (access(current_path, F_OK) != -1)
    return;

  int out = openat_i(pid, str_tmp_tracee);
  write_binary(pid, out, start, PAGESIZE);
  close_i(pid, out);
}

static void dump_handler(int pid, void *start_of_page) {
  debug_print("Dump handler detected access at %p\n", start_of_page);

  /* Unprotect Page */
  void * old_addr = ptrace_ripat(pid, writing_map);
  unprotect(pid, start_of_page, PAGESIZE);
  dump_page(pid, start_of_page);
  ptrace_ripat(pid, old_addr);
}

static void mru_handler(int pid, void *start_of_page) {
  debug_print("MRU Detected access at %p\n", start_of_page);

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

    debug_print("MRU Reprotecting page %p\n", pages_cache[last_page]);
 
    void * old_addr = ptrace_ripat(pid, writing_map);
    protect(pid, pages_cache[last_page], PAGESIZE);
    ptrace_ripat(pid, old_addr);

  }

  pages_cache[last_page] = start_of_page;
  last_page = (last_page + 1) % log_size;

#ifdef _DEBUG
  debug_print("%s", "     >>>> page cache\n");
  for (int i = 0; i < log_size; i++) {
    int c = (i + last_page) % log_size;
    debug_print("     >>>> %p\n", pages_cache[c]);
  }
#endif
}

static siginfo_t wait_sigtrap(pid_t child) {
  while (true) {
    siginfo_t sig = wait_process(child);

    if (sig.si_signo == SIGTRAP || sig.si_signo == (SIGTRAP|0x80)) {
      if (tracer_state == TRACER_LOCKED && is_hook_sigtrap(child)) {
	tracer_lock_range(child);
	continue;
      } else if (is_send_sigtrap(child)) {
	return sig;
      } else if (is_syscall_io(child)) {
	/* Make an handle IO */
	return sig;
      } else {
	  errx(EXIT_FAILURE, "Unknow syscallid of sigtrap : %d\n", get_syscallid(child)); 
      }
    }
    else if (sig.si_signo == SIGSEGV) {
      void *addr = round_to_page(sig.si_addr);
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
      ptrace_cont(child);
    } 
    else {
      errx(EXIT_FAILURE, "Error after lock_mem and before dump %s\n", strerror(sig.si_signo));
    }
  }
  debug_print("%s","\n");
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

static void receive_string_from_tracee(pid_t child, char *src_tracee, void *dst_tracer, size_t size) {
  wait_sigtrap(child);
  ptrace_getdata(child, (long long unsigned)src_tracee, dst_tracer, size);
  ptrace_cont(child);
}

static void tracer_lock_range(pid_t child) {
  
  debug_print("%s\n","START LOCK RANGE");

  assert(tracer_state == TRACER_LOCKED);

  ptrace_cont(child);
  void *from = (void*)receive_from_tracee(child);
  void *to = (void*)receive_from_tracee(child);
  /* We need that the process be stopped to protect */
  wait_process(child);

  long unsigned nb_pages_to_allocate = nb_pages_in_range(from, to);

  void * old_addr = ptrace_ripat(child, writing_map);
  for (long unsigned i = 0; i < nb_pages_to_allocate; i++)
    if (!is_mru(from + PAGESIZE * i))
      protect(child, round_to_page(from + PAGESIZE * i), PAGESIZE);
  ptrace_ripat(child, old_addr);

  ptrace_cont(child);

  debug_print("%s\n", "END LOCK RANGE");
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

void tracer_lock_mem(pid_t pid) {

  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);
  FILE *maps = fopen(maps_path, "r");

  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  debug_print("%s\n", "START LOCK MEM");

  void *addresses[65536];
  char buf[BUFSIZ + 1];
  int counter = 0;
  char *start_of_stack, *end_of_stack;

  while (fgets(buf, BUFSIZ, maps)) {
    void *start, *end;

    debug_print("%s",buf);
    sscanf(buf, "%p-%p", &start, &end);
    
    /* Special place where injected code will be put and execute */
    /* this memory need to be writable and executable */
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

  debug_print("%s\n", "END LOCK MEM");
}

void flush_hot_pages_trace_to_disk(pid_t pid) {

  debug_print("%s\n", "START FLUSH HOT PAGE");

  char path[MAX_PATH];
  char buf[14];
  
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

  debug_print("%s\n","END FLUSH HOT PAGE");
}

void dump_unprotected_pages(pid_t pid) {
  debug_print("%s\n", "START DUMP UNPROTECTED PAGE");

  /* Dump the unprotected pages before entering the codelet region. */
  for (int i = 0; i < log_size; i++) {
    int c = (i + last_page) % log_size;
    if (pages_cache[c] != 0) {
      dump_page(pid, pages_cache[c]);
    }
  }

  debug_print("%s\n", "END DUMP UNPROTECTED PAGE");
}

void dump_arg(pid_t pid) {

  printf("DUMP( %s %d count = %d) \n", loop_name, invocation, count);

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
  
  /* Wait for end of arguments sigtrap */
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

  receive_string_from_tracee(pid, str_tmp_tracee, loop_name, SIZE_LOOP);
  invocation = (int)receive_from_tracee(pid);
  count = (int)receive_from_tracee(pid);

  debug_print("LOOP NAME : %s\n",loop_name);
  debug_print("INVOCATION : %d\n",invocation);
  debug_print("COUNT : %d\n",count);
}

void tracer_dump(pid_t pid) {
  read_args(pid);
  dump_arg(pid);
}

void tracer_init(pid_t pid) {

  wait_process(pid);
  ptrace_attach(pid);
  create_dump_dir();
  ptrace_cont(pid);

  addr_state = (void*)receive_from_tracee(pid);
  size_state = (size_t)receive_from_tracee(pid);
  errnol = round_to_page((void*)receive_from_tracee(pid));
  str_tmp_tracee = (void*)receive_from_tracee(pid);
  writing_map = (void*)receive_from_tracee(pid);

  debug_print("STATE : %p\n", addr_state);
  debug_print("SIZE_STATE : %lu\n", size_state);
  debug_print("ERRNO_LOCATION : %p\n", errnol);
  debug_print("STRING TMP : %p\n", str_tmp_tracee);
  debug_print("%s\n", "TRACER INIT DONE");
}


int main(int argc, char *argv[]) {

  pid_t child = 0;

  child = atoi(argv[1]);

  tracer_init(child);
  debug_print("%s\n", "******* TRACER_UNLOCKED");
  tracer_state = TRACER_UNLOCKED;

  /* Wait for lock_mem trap */
  wait_sigtrap(child);
  debug_print("%s\n", "******* Start Locking");
  unprotect(child, round_to_page(writing_map), PAGESIZE);
  tracer_lock_mem(child);

  debug_print("%s\n", "******* TRACER_LOCKED");
  tracer_state = TRACER_LOCKED;
  ptrace_cont(child);

  /* Dump arguments  */
  tracer_dump(child);

  debug_print("%s\n", "******* TRACER_DUMPING");
  tracer_state = TRACER_DUMPING;
  ptrace_cont(child);

  while(1) {
    wait_sigtrap(child);
  }

  ptrace_detach(child);

  return EXIT_SUCCESS;
}

 
