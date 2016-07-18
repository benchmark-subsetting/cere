/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2016, Universite de Versailles St-Quentin-en-Yvelines       *
 *                                                                           *
 * CERE is free software: you can redistribute it and/or modify it under     *
 * the terms of the GNU Lesser General Public License as published by        *
 * the Free Software Foundation, either version 3 of the License,            *
 * or (at your option) any later version.                                    *
 *                                                                           *
 * CERE is distributed in the hope that it will be useful,                   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 * GNU General Public License for more details.                              *
 *                                                                           *
 * You should have received a copy of the GNU General Public License         *
 * along with CERE.  If not, see <http://www.gnu.org/licenses/>.             *
 *****************************************************************************/

#include "syscall_interface.h"
#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/user.h>
#include <syscall.h>

#include "ptrace.h"
#include "types.h"

#define _DEBUG 1
//#undef _DEBUG

#include "debug.h"

#ifdef __x86_64__

/* arch/ABI   arg1   arg2   arg3   arg4   arg5   arg6   arg7  */
/* ────────────────────────────────────────────────────────── */
/* x86_64     rdi    rsi    rdx    r10    r8     r9      -    */

static register_t inject_syscall(pid_t pid, int nb_args, register_t syscallid,
                                 ...) {

  /* We do the backup of registers */

  long r;
  register_t ret;
  struct user_regs_struct regs, regs_backup;

  ptrace_getregs(pid, &regs);
  regs_backup = regs;

  /* We get back arguments and adequately put them in registers */

  int i = 0;
  va_list vargs;
  va_start(vargs, syscallid);

  /* If we have more than 7 arguments, we must put them on the stack */
  /* For the moment, we don't handle this case  */
  long long unsigned *regs_ptr[] = {&(regs.rdi), &(regs.rsi), &(regs.rdx),
                                    &(regs.r10), &(regs.r8),  &(regs.r9)};
  size_t nb_max_args = sizeof(regs_ptr) / sizeof(register_t);
  assert(nb_max_args >= nb_args);

  regs.rax = syscallid;
  for (i = 0; i < nb_args; i++)
    *(regs_ptr[i]) = (register_t)va_arg(vargs, void *);
  va_end(vargs);

  /* We will inject syscall code */

  int len = 3;
  char backup_instr[len];

  /* 0f 05 syscall */
  /* cc    int $3 (SIGTRAP)  */
  char inject_instr[] = "\x0f\x05\xcc";

  ptrace_getdata(pid, regs.rip, backup_instr, len);
  ptrace_putdata(pid, regs.rip, inject_instr, len);

  ptrace_setregs(pid, &regs);

  ptrace_cont(pid);
  wait_process(pid);

  ptrace_getregs(pid, &regs);
  ret = regs.rax;
  ptrace_putdata(pid, regs_backup.rip, backup_instr, len);
  ptrace_setregs(pid, &regs_backup);

  return ret;
}

register_t get_arg_from_regs(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return regs.rdi;
}

int get_syscallid(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return -(int)regs.orig_rax;
}

bool is_valid_io(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  int syscallid = -regs.orig_rax;
  switch (syscallid) {
  case SYS_write:
    return (regs.rdi == fileno(stdout) || regs.rdi == fileno(stderr));
  case SYS_read:
  case SYS_open:
  case SYS_openat:
  case SYS_close:
    /* Add all ios forbidden */
    return false;
  default:
    return true;
  }
}

bool is_syscall_io(pid_t pid) {
  char name_syscall[16];
  int syscallid = get_syscallid(pid);
  switch (syscallid) {
  case SYS_read:
    strcpy(name_syscall, "read");
    break;
  case SYS_write:
    strcpy(name_syscall, "write");
    break;
  case SYS_open:
    strcpy(name_syscall, "open");
    break;
  case SYS_close:
    strcpy(name_syscall, "close");
    break;
  default:
    return false;
  }

  debug_print("Syscall IO detected : %s\n", name_syscall);
  return true;
}

void send_to_tracer(register_t arg) {
  asm volatile("mov %0,%%rax" : : "r"((register_t)SYS_send));
  asm volatile("mov %0,%%rdi" : : "r"(arg));
  sigtrap();
}

void inline sigtrap(void) { asm volatile("int $3"); }

void hook_sigtrap(void) {
  debug_print("%s\n", "Hook sigtrap !");
  asm volatile("mov %0,%%rax" : : "r"((register_t)SYS_hook));
  sigtrap();
}

bool is_hook_sigtrap(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return (regs.rax == SYS_hook);
}

bool is_send_sigtrap(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return (regs.rax == SYS_send);
}

#endif

#ifdef __arm__
void inject_syscall(int syscallid, pid_t tid,
                    struct user_regs_struct * regs->va_list vargs);
#endif

#ifdef __aarch64__
void inject_syscall(int syscallid, pid_t tid,
                    struct user_regs_struct * regs->va_list vargs);
#endif

void protect(pid_t pid, char *start, size_t size) {
  debug_print("TO BE PROTECTED :  %p (%lu)\n", start, size);
  register_t ret = inject_syscall(pid, 3, SYS_mprotect, start, size, PROT_NONE);
  /* We can try to protect a page that has been removed from memory */
  /* beetween the lock_mem() and dumping args */
  if (tracer_state == TRACER_LOCKED && ret == -ENOMEM)
    return;
  assert(ret == 0);
}

void unprotect(pid_t pid, char *start, size_t size) {
  debug_print("TO BE UNPROTECTED :  %p (%lu)\n", start, size);
  register_t ret = inject_syscall(pid, 3, SYS_mprotect, start, size,
                                  (PROT_READ | PROT_WRITE | PROT_EXEC));
  assert(ret == 0);
}

void write_page(pid_t pid, int fd, const void *buf, size_t nbyte) {
  debug_print("%s\n", "TO BE WROTE");
  register_t ret = inject_syscall(pid, 3, SYS_write, fd, buf, nbyte);
  assert((int)ret >= 0);
}

int openat_i(pid_t pid, char *pathname) {
  debug_print("%s\n", "TO BE OPEN");
  register_t ret = inject_syscall(pid, 4, SYS_openat, AT_FDCWD, pathname,
                                  (O_WRONLY | O_CREAT | O_EXCL), S_IRWXU);
  assert((int)ret > 0);
  return ret;
}

void close_i(pid_t pid, int fd) {
  debug_print("TO BE CLOSE :  %d\n", fd);
  register_t ret = inject_syscall(pid, 1, SYS_close, fd);
  assert(ret != -1L);
}
