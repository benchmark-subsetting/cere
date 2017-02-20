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
#ifndef __PTRACE__H
#define __PTRACE__H

#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/user.h>

#include <sys/uio.h>
#include <sys/ptrace.h>
#include <asm/ptrace.h>

#include "types.h"


void ptrace_cont(pid_t pid);
#if defined(__amd64__)
void ptrace_setregs(pid_t pid, struct user_regs_struct *regs);
void ptrace_getregs(pid_t pid, struct user_regs_struct *regs);
#elif defined(__aarch64__)
void ptrace_setregs(pid_t pid, struct user_pt_regs *regs);
void ptrace_getregs(pid_t pid, struct user_pt_regs *regs);
#endif

void ptrace_getdata(pid_t pid, long readAddr, char * readBuf, int size);
void ptrace_putdata(pid_t pid, long writeAddr, char * writeBuf, int size);
void ptrace_syscall(pid_t pid);

void follow_threads(pid_t pid);
event_t wait_event(pid_t wait_for);
void stop_all_except(pid_t pid);
void continue_all(void);

#endif /* __PTRACE__H */
