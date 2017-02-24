/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2016-2017, Universite de Versailles St-Quentin-en-Yvelines  *
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

#include <assert.h>
#include <fcntl.h>
#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/user.h>
#include <syscall.h>

#include "ptrace.h"
#include "tracer_interface.h"
#include "types.h"

#define _DEBUG 1
#undef _DEBUG

#include "debug.h"


extern struct tracer_buff_t * tracer_buff;

#if defined(__amd64__)
#define NB_MAX_ARGS 6

/* arch/ABI   arg1   arg2   arg3   arg4   arg5   arg6   arg7  */
/* x86_64     rdi    rsi    rdx    r10    r8     r9      -    */
static register_t inject_syscall(pid_t pid, int nb_args, register_t syscallid,
                                 ...) {

  /* If we have more than 6 arguments, we must put them on the stack */
  /* For the moment, we don't handle this case  */
  assert(NB_MAX_ARGS >= nb_args);

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

  long long unsigned *regs_ptr[NB_MAX_ARGS] = {&(regs.rdi), &(regs.rsi),
                                               &(regs.rdx), &(regs.r10),
                                               &(regs.r8),  &(regs.r9)};

  regs.rip = (register_t)tracer_buff->syscall;
  regs.rax = syscallid;

  if (syscallid == SYS_unprotect_protect) {
    regs.rip = (register_t)tracer_buff->unprotect_protect;
    regs.rax = SYS_mprotect;
    regs.r12 = SYS_mprotect;
    regs_ptr[3] = &(regs.r13);
    regs_ptr[4] = &(regs.r14);
    regs_ptr[5] = &(regs.r15);
  }

  for (i = 0; i < nb_args; i++) {
    *(regs_ptr[i]) = va_arg(vargs, long long unsigned);
  }
  va_end(vargs);

  ptrace_setregs(pid, &regs);

  ptrace_cont(pid);
  wait_event(pid);
  ptrace_getregs(pid, &regs);
  ret = regs.rax;

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
  return regs.orig_rax;
}

bool is_hook_sigtrap(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return (regs.rax == SYS_hook);
}

bool is_dump_sigtrap(pid_t pid) {
  struct user_regs_struct regs;
  ptrace_getregs(pid, &regs);
  return (regs.rax == SYS_dump);
}

void clear_trap(pid_t pid) {
}

#elif defined(__aarch64__)
#define NB_MAX_ARGS 6

/* arch/ABI   arg1   arg2   arg3   arg4   arg5   arg6   arg7  */
/* aarch64      x0     x1     x2     x3     x4     x5     -  */
static register_t inject_syscall(pid_t pid, int nb_args, register_t syscallid,
                                 ...) {

  /* For the moment, we don't handle this case  */
  assert(NB_MAX_ARGS >= nb_args);

  /* We do the backup of registers */
  long r;
  register_t ret;
  struct user_pt_regs regs, regs_backup;

  ptrace_getregs(pid, &regs);
  regs_backup = regs;

  /* We get back arguments and adequately put them in registers */

  int i = 0;
  va_list vargs;
  va_start(vargs, syscallid);

  long long unsigned *regs_ptr[NB_MAX_ARGS] = {&(regs.regs[0]), &(regs.regs[1]),
                                               &(regs.regs[2]), &(regs.regs[3]),
                                               &(regs.regs[4]), &(regs.regs[5]),
                                              };

  if (syscallid == SYS_unprotect_protect) {
    regs.pc = (register_t)tracer_buff->unprotect_protect;
    regs.regs[8] = SYS_mprotect;
    regs.regs[22] = SYS_mprotect;
    regs_ptr[3] = &(regs.regs[19]);
    regs_ptr[4] = &(regs.regs[20]);
    regs_ptr[5] = &(regs.regs[21]);
  } else {
    regs.pc = (register_t)tracer_buff->syscall;
    regs.regs[8] = syscallid;
  }

  for (i = 0; i < nb_args; i++) {
    register_t r = va_arg(vargs, long long unsigned);
    *(regs_ptr[i]) = r;
  }
  va_end(vargs);

  // prepare registers for syscall
  ptrace_setregs(pid, &regs);

  // execute syscall in tracee
  ptrace_cont(pid);

  // wait for sigtrap in tracee
  wait_event(pid);

  // restore old registers
  ptrace_getregs(pid, &regs);

  ret = regs.regs[0];

  ptrace_setregs(pid, &regs_backup);

  return ret;
}

register_t get_arg_from_regs(pid_t pid) {
  struct user_pt_regs regs;
  ptrace_getregs(pid, &regs);
  return regs.regs[0];
}

int get_syscallid(pid_t pid) {
  struct user_pt_regs regs;
  ptrace_getregs(pid, &regs);
  return regs.regs[8];
}

bool is_hook_sigtrap(pid_t pid) {
  struct user_pt_regs regs;
  ptrace_getregs(pid, &regs);
  return (regs.regs[8] == SYS_hook);
}

bool is_dump_sigtrap(pid_t pid) {
  struct user_pt_regs regs;
  ptrace_getregs(pid, &regs);
  return (regs.regs[8] == SYS_dump);
}

void clear_trap(pid_t pid) {
  struct user_pt_regs regs;
  ptrace_getregs(pid, &regs);
  regs.pc +=4;
  ptrace_setregs(pid, &regs);
}

void debug_regs(pid_t pid) {
  struct user_pt_regs regs;
  ptrace_getregs(pid, &regs);
  debug_print("register [pc] = %llx\n", regs.pc);
  for (int i = 0; i < 23; i++) {
    debug_print("register [%d] = %llx\n", i, regs.regs[i]);
  }
}

#endif

void protect_i(pid_t pid, char *start, size_t size) {
  debug_print("TO BE PROTECTED :  %p (%lu)\n", start, size);
  register_t ret = inject_syscall(pid, 3, SYS_mprotect, start, size,
          (long long unsigned) PROT_NONE);
  /* We can try to protect a page that has been removed from memory */
  /* beetween the lock_mem() and dumping args */
  if (tracer_state == TRACER_LOCKED && ret == -ENOMEM)
    return;

   if (ret != 0) {
    errx(EXIT_FAILURE, "Failed to protect page at %p with error %d\n",
         start, (int) ret);
  }
}

void unprotect_i(pid_t pid, char *start, size_t size) {
  debug_print("TO BE UNPROTECTED :  %p (%lu)\n", start, size);
  register_t ret = inject_syscall(pid, 3, SYS_mprotect, start, size,
                  (long long unsigned) (PROT_READ | PROT_WRITE | PROT_EXEC));
   if (ret != 0) {
    errx(EXIT_FAILURE, "Failed to unprotect page at %p with error %d\n",
         start, (int) ret);
  }
}

void unprotect_protect_i(pid_t pid, char *start_u, size_t size_u, char *start_p,
                       size_t size_p) {
  debug_print("TO BE UNPROTECTED :  %p (%lu) ... ", start_u, size_u);
  debug_print("TO BE PROTECTED :  %p (%lu)\n", start_p, size_p);
  register_t ret = inject_syscall(pid, 6, SYS_unprotect_protect,
                  start_p, size_p,
                  (long long unsigned) PROT_NONE,
                  start_u, size_u,
                  (long long unsigned) (PROT_READ | PROT_WRITE | PROT_EXEC));

  if (ret != 0) {
    errx(EXIT_FAILURE, "Failed to unprotect page at %p with error %d\n",
         start_u, (int)ret);
  }

  /* it is ok here for reprotect to fail, when we reprotect a page that has
     since been deallocated. Therefore we only check the result of unprotect.
     The target of unprotect cannot have been deallocated because it should be
     protected.
   */
}

void write_i(pid_t pid, int fd, const void *buf, size_t nbyte) {
  register_t ret = inject_syscall(pid, 3, SYS_write, fd, buf, nbyte);
  assert((int)ret >= 0);
}

int openat_i(pid_t pid, char *pathname) {
  register_t ret = inject_syscall(pid, 4, SYS_openat, AT_FDCWD, pathname,
                 (long long unsigned) (O_WRONLY | O_CREAT | O_EXCL), S_IRWXU);
  assert((int)ret > 0);
  return ret;
}

void close_i(pid_t pid, int fd) {
  register_t ret = inject_syscall(pid, 1, SYS_close, fd);
  assert(ret != -1L);
}
