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
#ifndef __SYSCALL_INTERFACE__H
#define __SYSCALL_INTERFACE__H

#include <stdbool.h>
#include <stddef.h>
#include <sys/types.h>

void protect(pid_t pid, char *start, size_t size);
void unprotect(pid_t pid, char *start, size_t size);
void unprotect_state(pid_t pid, char *start, size_t size);
void write_page(pid_t pid, int fd, const void *buf, size_t nbyte);
int openat_i(pid_t pid, char *pathname);
void close_i(pid_t pid, int fd);

register_t get_arg_from_regs(pid_t pid);
int get_syscallid(pid_t pid);
void send_to_tracer(register_t arg);
bool is_send_sigtrap(pid_t pid);
void hook_sigtrap(void);
bool is_hook_sigtrap(pid_t pid);
bool is_syscall_io(pid_t pid);
void sigtrap(void);

#endif /* __SYSCALL_INTERFACE__H */
