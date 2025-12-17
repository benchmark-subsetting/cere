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

#ifndef __TRACEE_INTERFACE__H
#define __TRACEE_INTERFACE__H

extern struct tracer_buff_t tracer_buff;

// These functions MUST be inline to avoid PLT/GOT access after memory is
// protected Definitions are here in the header to ensure inlining

#if defined(__amd64__)
static inline void
hook_sigtrap(void)
{
  asm volatile("mov %0,%%rax; int $3"
               :
               : "r"((register_t)SYS_hook)
               : "rax", "memory");
}

static inline void
send_to_tracer(register_t arg)
{
  asm volatile("mov %0,%%rax; mov %1,%%rdi; int $3"
               :
               : "r"((register_t)SYS_dump), "r"(arg)
               : "rax", "rdi", "memory");
}
#elif defined(__aarch64__)
static inline void
hook_sigtrap(void)
{
  asm volatile("mov x8, %0; brk #0"
               :
               : "r"((register_t)SYS_hook)
               : "x8", "memory");
}

static inline void
send_to_tracer(register_t arg)
{
  asm volatile("mov x8, %0; mov x0, %1; brk #0"
               :
               : "r"((register_t)SYS_dump), "r"(arg)
               : "x8", "x0", "memory");
}
#endif

#endif /* TRACEE_INTERFACE__H */
