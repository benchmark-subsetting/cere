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
#ifndef __TYPES__H
#define __TYPES__H

#include <limits.h>

#define PAST_INV 30
#define MAX_PATH 256
#define MAX_IGNORE 32
#define TRACE_SIZE (16384 * 100)
#define LOG_SIZE 64
#define SIZE_LOOP 256
#define CALLOC_INIT 512
#define OFFSET_STR 0x100 /* Offset for write string */

#define SYS_dump (INT_MAX-1)
#define SYS_hook (INT_MAX-2)

enum tracer_state_t {
  TRACER_UNLOCKED = 1,
  TRACER_LOCKED = 2,
  TRACER_DUMPING = 3
};

extern enum tracer_state_t tracer_state;

#endif /* __TYPES__H */
