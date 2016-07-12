#include <assert.h>
#include <err.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <string.h>
#include <signal.h>

#include "types.h"

#if __WORDSIZE == 64
#  define WORDSIZE 8
#elif __WORDSIZE == 32
#  define WORDSIZE 4
#endif

#define _DEBUG 1
#undef _DEBUG

#include "debug.h"

void ptrace_getsiginfo(pid_t pid, siginfo_t *sig) {
  if(ptrace(PTRACE_GETSIGINFO, pid, (void*)0 , sig) == -1)
    errx(EXIT_FAILURE, "ptrace(PTRACE_GETSIGINFO) : %s\n", strerror(errno));
}

void ptrace_cont(pid_t pid) {
  int r = 0;
  do {
    r = ptrace(PTRACE_CONT, pid, NULL, NULL);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH));
}

void ptrace_setregs(pid_t pid, struct user_regs_struct* regs){
  int r = 0;
  do {
    r = ptrace(PTRACE_SETREGS, pid, NULL, regs);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO || errno == ESRCH));
}

void ptrace_getregs(pid_t pid, struct user_regs_struct* regs){
  int r = 0;
  do {
    r = ptrace(PTRACE_GETREGS, pid, NULL, regs);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO || errno == ESRCH));
}

void ptrace_singlestep(pid_t pid) {
  int r = 0;
  do {
    r = ptrace(PTRACE_SINGLESTEP, pid, NULL, NULL);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == EIO || errno == ESRCH));
}

void ptrace_me(void) {
  if (ptrace(PTRACE_TRACEME, 0, 0, 0) == -1){
    errx(EXIT_FAILURE, "ptrace(PTRACE_ME) : %s\n", strerror(errno));
  }
}

void ptrace_interrupt(pid_t pid) {
  if (ptrace(PTRACE_INTERRUPT, pid, 0, 0) == -1) {
    errx(EXIT_FAILURE, "ptrace(PTRACE_INTERRUPT) : %s\n", strerror(errno));
  }
}

void attach_all_threads(pid_t tid[], int nbthread) {
  long t,r;
  unsigned long c = 0L;

  /* Attach all threads */
  for (t = 0; t < nbthread; t++) {
    do {
      r = ptrace(PTRACE_SEIZE, tid[t], (void *)0, PTRACE_O_TRACESYSGOOD);

#ifdef _DEBUG
      if (c++ % 10000000 == 0)
	debug_print("Try attach %d\n", tid[t]);
#endif

    } while (r == -1L && (errno == EBUSY || errno == EIO || errno == EFAULT || errno == ESRCH));

    if (r == -1L) {
      const int saved_errno = errno;
      while (t-- > 0)
	do {
	  r = ptrace(PTRACE_DETACH, tid[t], (void *)0, (void *)0);
	  debug_print("Detach of %d\n", tid[t]);
	} while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH));
      nbthread = 0;
      errno = saved_errno;
      break;
    }
  }
  debug_print("Attach to %d ... %d\n", tid[0], tid[nbthread-1]);
}

void detach_all_threads(pid_t tid[], int nbthread) {
  
  long t,r;
  unsigned long c = 0L;

  /* Detach from all tasks. */
  for (t = 0; t < nbthread; t++) {
    do {
      r = ptrace(PTRACE_DETACH, tid[t], (void *)0, (void *)0);

#ifdef _DEBUG
      if (c++ % 10000000 == 0)
	debug_print("Try detach %d\n", tid[t]);
#endif 

    } while (r == -1 && (errno == EBUSY || errno == EFAULT || errno != ESRCH));
  }
  debug_print("Detach of %d ... %d\n", tid[0], tid[nbthread-1]);
}

void ptrace_getdata(pid_t child, long addr, char *str, int len) {
  
  int save_errno = errno;

  union u {
    long val;
    char chars[WORDSIZE];
  }data;
  
  char *laddr;
  int i, j;
  i = 0;
  j = len / WORDSIZE;
  laddr = str;

  while(i < j) {
    save_errno = errno;
    data.val = ptrace(PTRACE_PEEKDATA, child, addr + i * WORDSIZE, NULL);
    if (data.val == -1 && save_errno != errno)
      errx(EXIT_FAILURE, "Error get data : %s\n",strerror(errno));
    memcpy(laddr, data.chars, WORDSIZE);
    ++i;
    laddr += WORDSIZE;
  }
  j = len % WORDSIZE;
  if(j != 0) {
    data.val = ptrace(PTRACE_PEEKDATA, child, addr + i * WORDSIZE, NULL);
    memcpy(laddr, data.chars, j);
  }
  str[len] = '\0';
}

void ptrace_attach(pid_t pid) {
  pid_t tids[] = {pid};
  attach_all_threads(tids, 1);
}

void ptrace_detach(pid_t pid) {
  pid_t tids[] = {pid};
  detach_all_threads(tids, 1);
}
  
void ptrace_putdata(pid_t child, long addr, char *str, int len)
{
  union u {
    long val;
    char chars[WORDSIZE];
  }data;
  
  char *laddr;
  int i, j;
  i = 0;
  j = len / WORDSIZE;
  laddr = str;
  while(i < j) {
    memcpy(data.chars, laddr, WORDSIZE);
    ptrace(PTRACE_POKEDATA, child, addr + i * WORDSIZE, data.val);
    ++i;
    laddr += WORDSIZE;
  }
  j = len % WORDSIZE;
  if(j != 0) {
    memcpy(data.chars, laddr, j);
    if (ptrace(PTRACE_POKEDATA, child, addr + i * WORDSIZE, data.val) == -1)
      errx(EXIT_FAILURE, "Error put data : %s\n", strerror(errno));
  }
}

siginfo_t wait_process(pid_t pid) {

  int r, status = 0;
  siginfo_t sig;
  
  do {
    r = waitpid(pid, &status, WSTOPPED);
  } while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH || errno == EIO));

  ptrace_getsiginfo(pid, &sig);
  int sicode = sig.si_code;

  if (WIFEXITED(status)) 
    errx(EXIT_FAILURE, "%d exit %d\n", pid, WEXITSTATUS(status));

  if (WIFSIGNALED(status)) 
    errx(EXIT_FAILURE,  "Process %d terminate by signal : %d\n", pid, WTERMSIG(status));

  if (WIFCONTINUED(status)) 
    errx(EXIT_FAILURE, "%d continue\n", pid);

  if (WIFSTOPPED(status)) {
    int num = WSTOPSIG(status);
    debug_print("Process %d stop by signal : %d -> ", pid, num);

    if (num == SIGSEGV) {
      if (sicode == SEGV_MAPERR)
	debug_print("SEGV_MAPERR address not mapped to object : %p\n", sig.si_addr);
      else if (sicode == SEGV_ACCERR)
	debug_print("SEGV_ACCERR invalid permissions for mapped object : %p\n", sig.si_addr);
      else
	errx(EXIT_FAILURE, "SEGV bad si_code : %d\n", sicode);
    } 
    else if (num == SIGTRAP || num == (SIGTRAP|0x80)) {
      if (sicode <= 0)
	debug_print("%s\n","SIGTRAP  was  delivered  as  a result of a user-space action");
      else if (sicode == 0x80)
	debug_print("%s\n", "SIGTRAP was sent by the kernel");
      else if (sicode == SIGTRAP || sicode == (SIGTRAP|0x80))
	debug_print("%s\n", "This is a syscall-stop");
      else 
	errx(EXIT_FAILURE, "TRAP bad si_code : %d\n", sicode);
    } 
    else if (num == SIGSTOP && tracer_state == 0)
      debug_print("%s\n", "SIGSTOP receive from tracee");
    else
      errx(EXIT_FAILURE, "??? signo : %d\n", num);
  }
  return sig;
}

void ptrace_listen(pid_t pid) {
  ptrace(PTRACE_LISTEN, pid, 0, 0);
}

void ptrace_syscall(pid_t pid) {
  int ret = ptrace(PTRACE_SYSCALL, pid, NULL, NULL);
  if (ret == -1)
    errx(EXIT_FAILURE, "PTRACE SYSCALL %s\n", strerror(errno));
}


void ptrace_syscall_flag(pid_t pid, int flag) {
  int ret = ptrace(PTRACE_SYSCALL, pid, NULL, flag);
  if (ret == -1)
    errx(EXIT_FAILURE, "PTRACE SYSCALL %s\n", strerror(errno));
}

void* ptrace_ripat(pid_t pid, void *addr) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  void *ret = (void*)regs.rip;
  regs.rip = (register_t)addr;
  ptrace_setregs(pid, &regs);
  return ret;
}

void show_registers(FILE *const out, pid_t tid, const char *const note)
{
  struct user_regs_struct regs;
  ptrace_getregs(tid, &regs);
#if (defined(__x86_64__) || defined(__i386__)) && __WORDSIZE == 64
  fprintf(out, "Task %d: RIP=%p, RSP=%p, RAX=%p, RDI=%p, RSI=%p, RDX=%p, ORIG_RAX=%p . %s\n", 
	  (int)tid, (void*)regs.rip, (void*)regs.rsp, (void*)regs.rax, (void*)regs.rdi, (void*)regs.rsi, (void*)regs.rdx, (void*)regs.orig_rax,  note);

#elif (defined(__arm__))
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp, note);
#elif (defined(__aarch64__)) 
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp, note);
#endif
}


void print_registers(FILE *const out, struct user_regs_struct *regs, const char *const note)
{
#if (defined(__x86_64__) || defined(__i386__)) && __WORDSIZE == 64
  fprintf(out, "RIP=%p, RSP=%p, RAX=%p, RDI=%p, RSI=%p, RDX=%p, ORIG_RAX=%p . %s\n", 
	   (void*)regs->rip, (void*)regs->rsp, (void*)regs->rax, (void*)regs->rdi, (void*)regs->rsi, (void*)regs->rdx, (void*)regs->orig_rax,  note);

#elif (defined(__arm__))
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp, note);
#elif (defined(__aarch64__)) 
  fprintf(out, "Task %d: EIP=%p, ESP=%p. %s\n", (int)tid, regs.ip, regs.sp, note);
#endif
}


void print_step(pid_t tid, pid_t tids[], int nbthread, int nb_step) {
  
  long r,s,t;
  struct user_regs_struct regs_bfr,regs_aft;

  for (s = 0; s < nb_step; s++) {
    ptrace_getregs(tid, &regs_bfr);
    ptrace_singlestep(tid);

    if (!r) {
      ptrace_getregs(tid, &regs_aft);
      for (t = 0; t < nbthread ; t++)
	show_registers(stderr, tids[t], "");
    } else {
      fprintf(stderr, "Single-step failed: %s.\n", strerror(errno));
    }
  }
}

void put_string(pid_t pid, char *src, void *dst, size_t nbyte) {
  debug_print("PUT STRING %s at %p\n", src, dst);
  ptrace_putdata(pid, (long long unsigned) dst, src, nbyte);
}
