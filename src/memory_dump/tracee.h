/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2016, Universite de Versailles St-Quentin-en-Yvelines  *
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
#ifndef __TRACEE__H
#define __TRACEE__H

#include <stdbool.h>
#include "types.h"

/* Public Interface for the memory tracer */

/* dump_init: initialises the memory tracer. Must be called at the very begin
 * of the tracee execution. Ideally it should be the first thing done in tha
 * tracee main() */
void dump_init(void);

/* dump_close: cleans-up the memory tracer. Must be called before exiting the
 * tracee. */
void dump_close(void);

/* dump: requests capture of a outlined region of interest. Must be called
 * before any other code in the function to be captured.
 *   - loop_name is the name of the region of interest
 *   - invocation is the target invocation that must be captured
 *   - arg_count is the number of arguments passed to the outlined function
 *   - ... are the arguments passed to the outlined function
 */
void dump(char *loop_name, int invocation, int arg_count, ...);

/* after_dump: terminates capture of a region of interest. must be called
   at the end of the function to be captured */
void after_dump(void);

/* mtrace_active: true when the memory protection hooks in hooks.c are
 * active */
extern bool mtrace_active;

#endif /* TRACEE__H */
