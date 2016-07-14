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
#include <string.h>
#include <err.h>

#include "../ccan/ccan/htable/htable.h"

#include "counters.h"

// Comparison function.
static bool streq(const void *e, void *string) {
  return strcmp(((struct region_counter *)e)->name, string) == 0;
}

static uint32_t hash_string(const char *string) {
  uint32_t ret;
  for (ret = 0; *string; string++)
    ret = (ret << 5) - ret + *string;
  return ret;
}

static size_t rehash(const void *e, void *unused) {
  return hash_string(((struct region_counter *)e)->name);
}

void init_counters(struct htable *counters) {
  htable_init(counters, rehash, NULL);
}

struct region_counter *get_region(struct htable *counters, char *loop_name) {
  struct region_counter *r = NULL;

  /* Try to get region */
  r = htable_get(counters, hash_string(loop_name), streq, loop_name);

  /* If it exists return it */
  if (r)
    return r;

  /* If it does not, we must create one */

  r = malloc(sizeof(*r));
  if (!r)
    errx(EXIT_FAILURE, "dump: Unable to allocate new region %s\n", loop_name);

  r->name = strdup(loop_name);
  if (!r)
    errx(EXIT_FAILURE, "dump: Unable to allocate new region %s\n", loop_name);

  /* Initialize call count to zero and add entry to hash table */
  r->call_count = 0;
  htable_add(counters, hash_string(r->name), r);
  return r;
}
