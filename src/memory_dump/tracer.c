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

#include "dump.h"
#include "counters.h"
#include "syscall_interface.h"
#include "pages.h"
#include "ptrace.h"

#include "../ccan/ccan/htable/htable.h"

#define MAX_PATH 256
#define MAX_IGNORE 32

#define _DEBUG 1

struct dump_state state __attribute__((aligned(PAGESIZE)));

void debug(const char msg[]) { syscall(SYS_write, 2, msg, strlen(msg)); }

bool is_mru(void *addr) {
  char *start_of_page = round_to_page(addr);
  bool present = false;
  for (int i = 0; i < state.log_size; i++) {
    if (state.pages_cache[i] == start_of_page) {
      present = true;
      break;
    }
  }
  return present;
}

void set_log_size(int log_size) {
  state.log_size = log_size;
}

void lock_mem(pid_t pid) {

  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);
  FILE *maps = fopen(maps_path, "r");

  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  char *addresses[65536];
  char buf[BUFSIZ + 1];
  int count = 0;

#ifdef _DEBUG
  fprintf(stderr, "***\tSTART LOCK MEM\t***\n");
#endif 

  while (fgets(buf, BUFSIZ, maps)) {
    off64_t start, end;

    /* fprintf(stderr, "%s", buf); */
    sscanf(buf, "%lx-%lx", &start, &end);

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

    char *page_start = round_to_page((char *)start);

    assert(count < 65536);
    addresses[count++] = (char *)page_start;
    addresses[count++] = (char *)end;
  }

  /* Protect all pages in adresses */
  while (count > 0) {
    char *end = addresses[--count];
    char *start = addresses[--count];
    protect(pid, start, (char*)(end - start));
  }

  /* Closing maps file must be done after locking the memory. If this close is
   * done before, it changes the memory mapping kept inside addresses making
   * the above SYS_mprotect call fail */
  fclose(maps);

  ptrace_cont(pid);

}

void flush_hot_pages_trace_to_disk(void) {
  char path[MAX_PATH];
  char buf[14];

  snprintf(path, sizeof(path), "%s/%s", state.dump_path, state.pagelog_suffix);

  int out = openat(AT_FDCWD, path, O_WRONLY | O_CREAT | O_EXCL, S_IRWXU);
  assert(out > 0);

  /* Dump the trace */
  for (int i = 0; i < TRACE_SIZE; i++) {
    int c = (i + state.last_trace) % TRACE_SIZE;
    if (state.pages_trace[c] != 0) {  
      snprintf(buf, sizeof(buf), "%s\n", state.pages_trace[c]);
      int wr = write(out, buf, sizeof(buf));
      assert(wr > 0);
    }
  }

  /* Dump the log */
  for (int i = 0; i < state.log_size; i++) {
    int c = (i + state.last_page) % state.log_size;
    if (state.pages_cache[c] != 0) {
      snprintf(buf, sizeof(buf), "%s\n", state.pages_trace[c]);
      int wr = write(out, buf, sizeof(buf));
      assert(wr > 0);
    }
  }
  close(out);
}

bool write_page(char path[], off64_t start) {

  int out = openat(AT_FDCWD, path, O_WRONLY | O_CREAT | O_EXCL, S_IRWXU);

  if (out <= 0) {
    /* region already dumped */
    if (errno == EEXIST)
      return false;
    else
      errx(EXIT_FAILURE, "Could not open %s : %s\n", path, strerror(errno)); 
  }

  int wr = write(out, (char *)start, PAGESIZE);
  if (wr < 0) {
    close(out);
    int result = unlinkat(AT_FDCWD, path, 0);
    assert(result != -1);
    return false;
  }
  assert(wr == PAGESIZE);

  close(out);
  return true;
}

bool dump_page(off64_t start) {
  /* Dump Page */
  char current_path[MAX_PATH];
  snprintf(current_path, sizeof(current_path), "%s/%p.memdump", state.dump_path, (void*)start);
  bool ret = write_page(current_path, start);
  return ret;
}

void dump_core(int count, void *addresses[]) {
  char path[MAX_PATH];
  snprintf(path, sizeof(path), "%s/%s", state.dump_path, state.core_suffix);

  FILE *core_map = fopen(path, "w");
  if (!core_map) {
    errx(EXIT_FAILURE, "Could not create core.map file : %s\n", strerror(errno));
  }

  int i;
  for (i = 0; i < count; i++) {
    fprintf(core_map, "%d %lx\n", i, (off64_t) addresses[i]);
  }
  fclose(core_map);
}

void tracer_dump(pid_t pid) {
  int SIZE_LOOP = 256;
  char loop_name[SIZE_LOOP];

  ptrace_syscall(pid);
  wait_process(pid);
  get_string(pid, loop_name, SIZE_LOOP);
  ptrace_syscall(pid);
  wait_process(pid);

  ptrace_syscall(pid);
  wait_process(pid);
  int invocation = (int)get_arg_fake_syscall(pid);
  ptrace_syscall(pid);
  wait_process(pid);

  ptrace_syscall(pid);
  wait_process(pid);
  int count = (int)get_arg_fake_syscall(pid);
  ptrace_syscall(pid);
  wait_process(pid);
  
  /* get region */
  struct region_counter *region = get_region(loop_name);
  /* increment call_count */
  region->call_count++;

  if (invocation <= PAST_INV && region->call_count == 1 || region->call_count == invocation - PAST_INV) {
    lock_mem(pid);
  }
  
  if (region->call_count != invocation) {
    ptrace_syscall(pid);
    wait_process(pid);
    bool* wrong_invocation = (bool*)get_arg_fake_syscall(pid);
    set_var(pid, wrong_invocation, false);

    ptrace_cont(pid);
    return;
  }

  if (region->call_count == invocation) {
    ptrace_syscall(pid);
    wait_process(pid);
    bool* wrong_invocation = (bool*)get_arg_fake_syscall(pid);
    set_var(pid, wrong_invocation, (void*)true);

    int i;
    void* addresses[count];
    for (i = 0; i < count; i++){
      ptrace_syscall(pid);
      wait_process(pid);
      addresses[i] = round_to_page(get_arg_fake_syscall(pid));
      ptrace_syscall(pid);
      wait_process(pid);
    }

    for (i = 0; i < count ; i++) {
      if (addresses[i])
	dump_page((off64_t) addresses[i]);
    }

    dump_core(count, addresses);
  }

  printf("DUMP( %s %d count = %d) \n", loop_name, invocation, count);

  /* Ensure that the dump directory exists */
  snprintf(state.dump_path, sizeof(state.dump_path), "%s/%s/%s",
	   state.dump_prefix, state.dump_root, loop_name);

  mkdir(state.dump_path, 0777);

  snprintf(state.dump_path, sizeof(state.dump_path), "%s/%s/%s/%d",
	   state.dump_prefix, state.dump_root, loop_name, invocation);

  if (mkdir(state.dump_path, 0777) != 0)
    errx(EXIT_FAILURE, "dump %s already exists, stop\n", state.dump_path);

  /*Dump hotpages to disk*/
  flush_hot_pages_trace_to_disk();

  char lel_bin_path[1024];

  /*Link to the original binary*/
  snprintf(lel_bin_path, sizeof(lel_bin_path), "%s/lel_bin", state.dump_path);
  int res = syscall(SYS_linkat, AT_FDCWD, "lel_bin", AT_FDCWD, lel_bin_path,
		    AT_SYMLINK_FOLLOW);
  if (res == -1)
    errx(EXIT_FAILURE, "Error copying the dump binary\n");

}

void mru_handler(int pid, char *start_of_page) {
  int esaved = errno;

#ifdef _DEBUG
  fprintf(stderr, "MRU Detected access at %p", start_of_page);
#endif

  bool present = is_mru(start_of_page);
  assert(present == false);

  /* Unprotect Page */
  protect(pid, start_of_page, (void*)PAGESIZE);

  /* Add page to page cache */
  /* we need to evict one of the pages, reprotect it ! */
  if (state.pages_cache[state.last_page] != 0) {

    /* add the evicted page to the trace */
    state.pages_trace[state.last_trace] = state.pages_cache[state.last_page];
    state.last_trace = (state.last_trace + 1) % TRACE_SIZE;

#ifdef _DEBUG
    fprintf(stderr, "MRU Reprotecting page %p\n", state.pages_cache[state.last_page]);
#endif
    protect(pid, state.pages_cache[state.last_page], (void*)PAGESIZE);
  }

  state.pages_cache[state.last_page] = start_of_page;
  state.last_page = (state.last_page + 1) % state.log_size;

#ifdef _DEBUG
  fprintf(stderr, "     >>>> page cache\n");
  for (int i = 0; i < state.log_size; i++) {
    int c = (i + state.last_page) % state.log_size;
    fprintf(stderr, "     >>>> %p\n", state.pages_cache[c]);
  }
#endif
  errno = esaved;
}

int main(int argc, char *argv[])
{
  pid_t child = 0;
  int status = 0;
  
  child = atoi(argv[1]);
  ptrace_attach(child);

  /* Wait dump */
  wait_process(child);
  
  while (1) {
    siginfo_t sig;
    char *addr = NULL;

    wait_process(child);
    ptrace_getsiginfo(child, &sig);
    addr = round_to_page(sig.si_addr);
    mru_handler(child, addr);
    ptrace_cont(child);
  }

  ptrace_detach(child);
    
  return status;
}

 
