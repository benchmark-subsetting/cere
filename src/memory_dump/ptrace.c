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

#if __WORDSIZE == 64
#  define WORDSIZE 8
#elif __WORDSIZE == 32
#  define WORDSIZE 4
#endif

#define _DEBUG 

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

void attach_all_threads(pid_t tid[], int nbthread) {
  long t,r;
  unsigned long c = 0L;
  for (t = 0; t < nbthread; t++) {
    do {
      r = ptrace(PTRACE_SEIZE, tid[t], (void *)0, PTRACE_O_EXITKILL | PTRACE_O_TRACESYSGOOD);

#ifdef _DEBUG
      if (c++ % 10000000 == 0)
	fprintf(stderr, "Try attach %d\n", tid[t]);
#endif 

    } while (r == -1L && (errno == EBUSY || errno == EIO || errno == EFAULT || errno == ESRCH));

    if (r == -1L) {
      const int saved_errno = errno;
      while (t-- > 0)
	do {
	  r = ptrace(PTRACE_DETACH, tid[t], (void *)0, (void *)0);
	  fprintf(stderr, "Detach of %d\n", tid[t]);
	} while (r == -1L && (errno == EBUSY || errno == EFAULT || errno == ESRCH));
      nbthread = 0;
      errno = saved_errno;
      break;
    }
  }

#ifdef _DEBUG
  fprintf(stderr, "Attach to %d ... %d\n", tid[0], tid[nbthread-1]);
#endif

}

void ptrace_attach(pid_t pid) {
  pid_t tids[] = {pid};
  attach_all_threads(tids, 1);
}

void ptrace_detach(pid_t pid) {
  pid_t tids[] = {pid};
  detach_all_threads(tids, 1);
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
	fprintf(stderr, "Try detach %d\n", tid[t]);
#endif 

    } while (r == -1 && (errno == EBUSY || errno == EFAULT || errno == ESRCH));
  }

#ifdef _DEBUG
  fprintf(stderr, "Detach of %d ... %d\n", tid[0], tid[nbthread-1]);
#endif 
}

void ptrace_getdata(pid_t child, long addr, char *str, int len) {
  
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
    data.val = ptrace(PTRACE_PEEKDATA, child, addr + i * WORDSIZE, NULL);
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
    ptrace(PTRACE_POKEDATA, child, addr + i * WORDSIZE, data.val);
  }
}

void wait_process(pid_t pid) {

  siginfo_t sig;
  waitid(P_PID, pid, &sig, WSTOPPED);

#ifdef _DEBUG
  fprintf(stderr, "Signal receive from %d %d\n", pid, sig.si_status);
#endif 

}

void ptrace_syscall(pid_t pid) {
  int ret = ptrace(PTRACE_SYSCALL, pid, NULL, NULL);
  assert(ret != -1);
}


void show_registers(FILE *const out, pid_t tid, const char *const note)
{
  struct user_regs_struct regs;
  long r;
  int i;

  union u {
    long val;
    char chars[WORDSIZE];
  }data;

  ptrace_getregs(tid, &regs);

#if (defined(__x86_64__) || defined(__i386__)) && __WORDSIZE == 64
  if (note && *note)
    fprintf(out, "Task %d: RIP=0x%016llx, RSP=0x%016llx. %s\n", (int)tid, regs.rip, regs.rsp, note);
  else
    fprintf(out, "Task %d: RIP=0x%016llx, RSP=0x%016llx.\n ", (int)tid, regs.rip, regs.rsp);

  ptrace_getdata(tid, regs.rip, (char*)&data.val, 1);

  for(i = 0; i < 8; i++)
    fprintf(out, "%02hhX ", data.chars[i]);
  fprintf(out, "\n");

#elif (defined(__arm__))
  if (note && *note)
    fprintf(out, "Task %d: EIP=0x%08xx, ESP=0x%08x. %s\n", (int)tid, regs.ip, regs.sp, note);
  else
    fprintf(out, "Task %d: EIP=0x%08xx, ESP=0x%08x.\n", (int)tid, regs.ip, regs.sp);

#elif (defined(__aarch64__)) 
  if (note && *note)
    fprintf(out, "Task %d: EIP=0x%08xx, ESP=0x%08x. %s\n", (int)tid, regs.ip, regs.sp, note);
  else
    fprintf(out, "Task %d: EIP=0x%08xx, ESP=0x%08x.\n", (int)tid, regs.ip, regs.sp);

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
