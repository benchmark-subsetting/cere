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
  asm volatile("mov %0,%%rsi" : : "r"((register_t)SYS_hook));
  sigtrap();
}

void send_to_tracer(register_t arg) {
  /* rdi and rsi are callee save */
  asm volatile("mov %0,%%rsi" : : "r"((register_t)SYS_dump));
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
#elif defined(__aarch64__)
static inline void sigtrap(void) {
  asm volatile("brk #0");
}

void hook_sigtrap(void) {
  asm volatile("mov x1, %0" : : "r"((register_t)SYS_hook) : "x1");
  sigtrap();
}

void send_to_tracer(register_t arg) {
  asm volatile("mov x1, %0" : : "r"((register_t)SYS_dump) : "x1");
  asm volatile("mov x0, %0" : : "r"((register_t)arg) : "x0");
  sigtrap();
}

struct tracer_buff_t tracer_buff = {
  .syscall = "\x01\x00\x00\xd4"           /* svc #0 syscall     */
             "\x00\x00\x20\xd4",          /* brk #0 (SIGTRAP)   */
  .unprotect_protect = "\x01\x00\x00\xd4" /* syscall protect    */
                       "\xe0\x03\x13\xaa" /* mov	x0, x19 */
                       "\xe1\x03\x14\xaa" /* mov	x1, x20 */
                       "\xe2\x03\x15\xaa" /* mov	x2, x21 */
                       "\xe8\x03\x16\xaa" /* mov	x8, x22 */
                       "\x01\x00\x00\xd4" /* syscall unprotect  */
                       "\x00\x00\x20\xd4" /* brk #0 (SIGTRAP)   */
};

#endif
