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

#include <sys/types.h>
#include "types.h"

#if defined(__amd64__)
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
  .syscall = {0x0f, 0x05,                 /* syscall           */
              0xcc},                      /* int $3 (SIGTRAP)  */
  .unprotect_protect = {0x0f, 0x05,       /* syscall protect   */
                        0x4c, 0x89, 0xe0, /* mov    %r12,%rax  */
                        0x4c, 0x89, 0xef, /* mov    %r13,%rdi  */
                        0x4c, 0x89, 0xf6, /* mov    %r14,%rsi  */
                        0x4c, 0x89, 0xfa, /* mov    %r15,%rdx  */
                        0x0f, 0x05,       /* syscall unprotect */
                        0xcc}             /* int $3 (SIGTRAP)  */
};
#elif defined(__aarch64__)
static inline void sigtrap(void) {
  asm volatile("brk #0");
}

void hook_sigtrap(void) {
  asm volatile("mov x8, %0" : : "r"((register_t)SYS_hook) : "x8");
  sigtrap();
}

void send_to_tracer(register_t arg) {
  asm volatile("mov x8, %0" : : "r"((register_t)SYS_dump) : "x8");
  asm volatile("mov x0, %0" : : "r"((register_t)arg) : "x0");
  sigtrap();
}

struct tracer_buff_t tracer_buff = {
  .syscall = {0x01, 0x00, 0x00, 0xd4,            /* svc #0 syscall     */
              0x00, 0x00, 0x20, 0xd4},           /* brk #0 (SIGTRAP)   */
  .unprotect_protect = {0x01, 0x00, 0x00, 0xd4,  /* syscall protect    */
                        0xe0, 0x03, 0x13, 0xaa,  /* mov	x0, x19 */
                        0xe1, 0x03, 0x14, 0xaa,  /* mov	x1, x20 */
                        0xe2, 0x03, 0x15, 0xaa,  /* mov	x2, x21 */
                        0xe8, 0x03, 0x16, 0xaa,  /* mov	x8, x22 */
                        0x01, 0x00, 0x00, 0xd4,  /* syscall unprotect  */
                        0x00, 0x00, 0x20, 0xd4}  /* brk #0 (SIGTRAP)   */
};

#endif
