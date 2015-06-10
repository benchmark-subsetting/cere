/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2015, Universite de Versailles St-Quentin-en-Yvelines  *
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
#include <linux/limits.h>
#include "../ccan/ccan/htable/htable.h"

#define BUF_SIZE 64

static struct htable regionHtab;
pid_t tracing = 0;
unsigned max_str_size = 0;

void init_regions(char*);
void init_io_detection(char*);
void start_io_detection(char*);
void stop_io_detection(char*);
void close_io_detection();

static uint32_t hash_string(const char*);

static bool streq(const void*, void*);
static size_t rehash(const void*, void*);

typedef struct
{
  char *name;
  int nb_invoc;
  int current_invocation;
  int *requested_invocations;
  pid_t pid;
  bool strace;
} region;
