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

void ptrace_getsiginfo(pid_t pid, siginfo_t *sig);
void ptrace_cont(pid_t pid);
void ptrace_listen(pid_t pid);
void ptrace_interrupt(pid_t pid);
void ptrace_setregs(pid_t pid, struct user_regs_struct *regs);
void ptrace_getregs(pid_t pid, struct user_regs_struct *regs);
void ptrace_attach(pid_t pid);
void ptrace_detach(pid_t pid);
void ptrace_me(void);
void ptrace_singlestep(pid_t pid);
void attach_all_threads(pid_t tasks[], int nbthread);
void detach_all_threads(pid_t tasks[], int nbthread);
void ptrace_getdata(pid_t pid, long readAddr, char *readBuf, int size);
void ptrace_putdata(pid_t pid, long writeAddr, char *writeBuf, int size);
void ptrace_syscall(pid_t pid);
void ptrace_syscall_flag(pid_t pid, int flag);
void *ptrace_ripat(pid_t pid, void *addr);
siginfo_t wait_process(pid_t pid);
void put_string(pid_t pid, char *src, void *dst, size_t nbyte);

/* Debug functions */
void print_registers(FILE *const out, struct user_regs_struct *regs,
                     const char *const note);
void show_registers(FILE *const out, pid_t tid, const char *const note);
void print_step(pid_t tid, pid_t tids[], int nbthread, int nb_step);

#endif /* __PTRACE__H */
