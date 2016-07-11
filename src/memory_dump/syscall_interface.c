#include <assert.h>
#include <sys/syscall.h>
#include <stdbool.h>
#include <stdarg.h>
#include <fcntl.h>
#include <sys/mman.h>

#include "types.h"
#include "pages.h"
#include "ptrace.h"
#include "syscall_interface.h"

#define _DEBUG 1
/* #undef _DEBUG */

#ifdef __x86_64__
static register_t inject_syscall(int syscallid, pid_t tid, struct user_regs_struct *regs, va_list vargs) {

  switch (syscallid) {

  case PROTECT:
    regs->rax = SYS_mprotect;
    regs->rdi = (register_t) va_arg(vargs, void *);
    regs->rsi = (register_t) va_arg(vargs, void *);
    regs->rdx = 0x0;
    break;

  case UNPROTECT:
    regs->rax = SYS_mprotect;
    regs->rdi = (register_t) va_arg(vargs , void *);
    regs->rsi = (register_t) va_arg(vargs , void *);
    regs->rdx = 0x7;
    break;

  case UNPROTECT_STATE:
    regs->rax = SYS_mprotect;
    regs->rdi = (register_t)va_arg(vargs , void *);
    regs->rsi = (register_t)va_arg(vargs , void *);
    regs->rdx = (PROT_READ | PROT_WRITE);
    break;

  case OPENAT:
    regs->rax = SYS_openat;
    regs->rdi = AT_FDCWD;
    regs->rsi = (register_t) va_arg(vargs, void *);
    regs->rdx = (O_WRONLY | O_CREAT | O_EXCL);
    regs->r10 = S_IRWXU;
    break;

  case WRITE:
    regs->rax = SYS_write;
    regs->rdi = (register_t)va_arg(vargs, void *);
    regs->rsi = (register_t) va_arg(vargs, void *);
    regs->rdx = (register_t) va_arg(vargs, void *);
    break;

  case CLOSE:
    regs->rax = SYS_close;
    regs->rdi = (register_t) va_arg(vargs, void *);
    break;

  default:
    errx(EXIT_FAILURE, "Bad syscall id %d\n", syscallid);
  }

  int len = 3;
  register_t rip_backup = regs->rip;
  char backup_instr[len];

  /* 0f 05 syscall          */
  /* cc    int $3 (SIGTRAP) */
  char inject_instr[] = "\x0f\x05\xcc";

  ptrace_getdata(tid, regs->rip, backup_instr, len);
  ptrace_putdata(tid, regs->rip, inject_instr, len);
  
  ptrace_setregs(tid,regs);

  ptrace_cont(tid);
  wait_process(tid);

  ptrace_getregs(tid, regs);

  switch (syscallid) {

  case PROTECT:
    /* We can try to protect a page that has been remove from memory */
    /* beetween the lock_mem() and dumping args  */
    if (tracer_state == TRACER_LOCKED && regs->rax == -ENOMEM)
      break;
  case UNPROTECT:
  case UNPROTECT_STATE:
    assert(regs->rax == 0);
    break;

  case WRITE:
    assert((int)regs->rax >= 0);
    break;

  case OPENAT:
    assert((int)regs->rax > 0);
    break;

  case CLOSE:
    assert(regs->rax != -1L);
    break;

  default:
    errx(EXIT_FAILURE, "Bad syscall id %d\n", syscallid);
  }

  int ret = regs->rax;
  ptrace_putdata(tid, rip_backup, backup_instr, len);

  return ret;
}

register_t get_arg_from_regs(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return regs.rdi;
}

void send_to_tracer(register_t arg) {
  asm volatile ("mov %0,%%rax" : : "r" ((register_t)SYS_send));
  asm volatile ("mov %0,%%rdi" : : "r" (arg));
  sigtrap();
}

void inline sigtrap(void) {
  asm volatile ("int $3");
}

void hook_sigtrap(void) {
#ifdef _DEBUG
  fprintf(stderr, "Hook sigtrap ! \n");
#endif
  asm volatile ("mov %0,%%rax" : : "r" ((register_t)SYS_hook));
  sigtrap();
}

bool is_hook_sigtrap(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return (regs.rax == SYS_hook);
}

#endif

#ifdef __arm__
void inject_syscall(int syscallid, pid_t tid, struct user_regs_struct *regs-> va_list vargs);
#endif

#ifdef __aarch64__
void inject_syscall(int syscallid, pid_t tid, struct user_regs_struct *regs-> va_list vargs);
#endif

static register_t injection_code(syscallID syscallid, pid_t tid, int nargs, ...) {

  long r;
  register_t ret;
  struct user_regs_struct regs, regs_backup;
  
  ptrace_getregs(tid, &regs);
  regs_backup = regs;

  int i;
  va_list vargs;
  va_start(vargs, nargs);
  ret = inject_syscall(syscallid, tid, &regs, vargs);
  va_end(vargs);

  ptrace_setregs(tid, &regs_backup);
  
  return ret;
}

void protect(pid_t pid, char* start, size_t size) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE PROTECTED :  %p\n", start);
#endif
  injection_code(PROTECT, pid, 2, start, size);
}

void unprotect(pid_t pid, char* start, size_t size) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE UNPROTECTED :  %p\n", start);
#endif
  injection_code(UNPROTECT, pid, 2, start, size);
}
void unprotect_state(pid_t pid, char* start, size_t size) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE UNPROTECTED :  %p\n", start);
#endif
  injection_code(UNPROTECT_STATE, pid, 2, start, size);
}

void write_page(pid_t pid, int fd, const void *buf, size_t nbyte){
#ifdef _DEBUG
  fprintf(stderr, "TO BE WROTE\n");
#endif
  injection_code(WRITE, pid, 2, fd, buf, nbyte);
}

int openat_i(pid_t pid, char *pathname) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE OPEN\n");
#endif  
  return (int) injection_code(OPENAT, pid, 1, pathname);
}

void close_i(pid_t pid, int fd) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE CLOSE :  %d\n", fd);
#endif  
  injection_code(CLOSE, pid, 1, fd);
}
