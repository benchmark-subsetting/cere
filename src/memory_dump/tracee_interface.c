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

#include <sys/types.h>
#include "types.h"

static inline void sigtrap(void) {
  asm volatile("int $3");
}

void hook_sigtrap(void) {
  asm volatile("mov %0,%%rax" : : "r"((register_t)SYS_hook));
  sigtrap();
}

void send_to_tracer(register_t arg) {
  asm volatile("mov %0,%%rax" : : "r"((register_t)SYS_dump));
  asm volatile("mov %0,%%rdi" : : "r"(arg));
  sigtrap();
}

struct tracer_buff_t tracer_buff = {
  .syscall = "\x0f\x05"               /* syscall           */
             "\xcc",                  /* int $3 (SIGTRAP)  */
  .unprotect_protect = "\x0f\x05"     /* syscall protect   */
                       "\x4c\x89\xe0" /* mov    %r12,%rax  */
                       "\x4c\x89\xef" /* mov    %r13,%rdi  */
                       "\x4c\x89\xf6" /* mov    %r14,%rsi  */
                       "\x4c\x89\xfa" /* mov    %r15,%rdx  */
                       "\x0f\x05"     /* syscall unprotect */
                       "\xcc"         /* int $3 (SIGTRAP)  */
};
