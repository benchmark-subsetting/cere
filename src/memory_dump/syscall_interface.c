#include <assert.h>
#include <sys/syscall.h>
#include <stdarg.h>

#include "ptrace.h"
#include "syscall_interface.h"

#define _DEBUG 1
#define SYS_FAKE (-1)
#define LOOP_SIZE 256

#ifdef __x86_64__
void inject_syscall(int syscallid, pid_t tid, struct user_regs_struct *regs, va_list vargs) {

  if ( syscallid == PROTECT ) {
    regs->rax = SYS_mprotect;
    regs->rdi = (long long unsigned) va_arg(vargs, void *);
    regs->rsi = (long long unsigned) va_arg(vargs, void *);
    regs->rdx = 0x0;
  } else if ( syscallid == UNPROTECT ) {
    regs->rax = SYS_mprotect;
    regs->rdi = (long long unsigned) va_arg(vargs , void *);
    regs->rsi = (long long unsigned) va_arg(vargs , void *);
    regs->rdx = 0x7;
  }

  int len = 3;
  long long unsigned rip_backup = regs->rip;
  char backup_instr[3];

  /* 0f 05 syscall          */
  /* cc    int $3 (SIGTRAP) */
  char inject_instr[] = "\x0f\x05\xcc";

  ptrace_getdata(tid, regs->rip, backup_instr, len);
  ptrace_putdata(tid, regs->rip, inject_instr, len);
  
  ptrace_setregs(tid, regs);

  ptrace_cont(tid);
  wait_process(tid);
  
  ptrace_getregs(tid, regs);
  assert(regs->rax == 0);

  ptrace_putdata(tid, rip_backup, backup_instr, len);
}

void* get_arg_fake_syscall(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  assert(regs.rax == SYS_fake);
  return (void*)regs.rdi;
}

void get_string(pid_t pid, char *str, size_t nbyte) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  ptrace_getdata(pid, regs.rdi, str, nbyte);
}

void sigtrap() {
  asm volatile ("int $3");
}

void set_var(pid_t pid, void *arg, void *val) {
  ptrace_putdata(pid, arg, val, sizeof(val));
}

#endif

#ifdef __arm__
void inject_syscall(int syscallid, pid_t tid, struct user_regs_struct *regs, va_list vargs);
#endif

#ifdef __aarch64__
void inject_syscall(int syscallid, pid_t tid, struct user_regs_struct *regs, va_list vargs);
#endif

void injection_code(syscallID syscallid, pid_t tid, int nargs, ...) {

  int status = 0;

  long r;
  struct user_regs_struct regs, regs_backup;
  
  ptrace_getregs(tid, &regs);
  regs_backup = regs;

  int i;
  va_list vargs;
  va_start(vargs, nargs);
  
  switch(syscallid) {
  case PROTECT:
  case UNPROTECT:
    /* regs.rip = (long long unsigned)findFunction(tid, "inject_syscall"); */
    inject_syscall(syscallid, tid, &regs, vargs);
    break;
  }

  ptrace_setregs(tid, &regs_backup);
  va_end(vargs);
}

void protect(pid_t pid, char* start, char* end) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE PROTECTED :  %p\n", start);
#endif

  injection_code(PROTECT, pid, 2, start, end);
}

void unprotect(pid_t pid, char* start, char *end) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE UNPROTECTED :  %p\n", start);
#endif

  injection_code(UNPROTECT, pid, 2, start, end);
}

void fake_syscall(void *arg) {
  syscall(SYS_fake, arg);
}
