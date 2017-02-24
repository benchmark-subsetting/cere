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
#ifndef __TYPES__H
#define __TYPES__H

#include <limits.h>
#include <sys/types.h>

#define MAX_TIDS 2048

#define PAST_INV 30
#define MAX_PATH 256
#define MAX_IGNORE 32
#define TRACE_SIZE (16384 * 100)
#define LOG_SIZE 64
#define SIZE_LOOP 256
#define CALLOC_INIT 512

#define SYS_dump (INT_MAX - 1)
#define SYS_hook (INT_MAX - 2)
#define SYS_unprotect_protect (INT_MAX - 3)

#define TRAP_LOCK_MEM 111
#define TRAP_START_ARGS 112
#define TRAP_END_ARGS 113

#if defined(__amd64__)
#define SIZE_SYSCALL_BIN (3)
#define SIZE_UNPROTECT_PROTECT_BIN (23)
#elif defined(__aarch64__)
#define SIZE_SYSCALL_BIN (8)
#define SIZE_UNPROTECT_PROTECT_BIN (28)
#endif

enum tracer_state_t {
  TRACER_UNLOCKED = 1,
  TRACER_FIRSTTOUCH = 2,
  TRACER_LOCKED = 3,
  TRACER_DUMPING = 4
};

extern enum tracer_state_t tracer_state;

struct tracer_buff_t {
  char syscall[SIZE_SYSCALL_BIN];
  char unprotect_protect[SIZE_UNPROTECT_PROTECT_BIN];
  char str_tmp[MAX_PATH];
} __attribute__ ((aligned(8), packed));

typedef struct {
  int signo;
  void * sigaddr;
  pid_t tid;
} event_t;

#endif /* __TYPES__H */
