/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2016, Universite de Versailles St-Quentin-en-Yvelines  *
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
#ifndef __PTRACE__H
#define __PTRACE__H

#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/user.h>

/* Wrapper interface for ptrace calls.
 * The following functions, wrap the equivalent ptrace
 * calls and perform error checking */
void ptrace_getsiginfo(pid_t pid, siginfo_t *sig);
void ptrace_cont(pid_t pid);
void ptrace_listen(pid_t pid);
void ptrace_interrupt(pid_t pid);
void ptrace_setregs(pid_t pid, struct user_regs_struct *regs);
void ptrace_getregs(pid_t pid, struct user_regs_struct *regs);
void ptrace_attach(pid_t pid);
void ptrace_singlestep(pid_t pid);

/* ptrace_getdata and ptrace_putdata write and read in the memory of a
 * tracee process */
void ptrace_getdata(pid_t pid, long readAddr, char *readBuf, int size);
void ptrace_putdata(pid_t pid, long writeAddr, char *writeBuf, int size);

void put_string(pid_t pid, char *src, void *dst, size_t nbyte);

/* ptrace_ripat: sets the rip register */
void *ptrace_ripat(pid_t pid, void *addr);

void ptrace_syscall(pid_t pid);

pid_t wait_process(pid_t pid, siginfo_t * siginfo);

void stop_all_except(pid_t pid);
void continue_all(void);

/* Debug functions */
void print_registers(FILE *const out, struct user_regs_struct *regs,
                     const char *const note);
void show_registers(FILE *const out, pid_t tid, const char *const note);

#endif /* __PTRACE__H */
