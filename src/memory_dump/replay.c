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
#define _LARGEFILE64_SOURCE
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#include <assert.h>
#include <err.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include "pages.h"

#define CACHE_SIZE_MB 20

long PAGESIZE;

void cacheflush(void) {
  const size_t size = CACHE_SIZE_MB * 1024 * 1024;
  int i, j;
  char *c = malloc(size);
  for (i = 0; i < 5; i++)
    for (j = 0; j < size; j++)
      c[j] = i * j;
}

/**********************************************************************
 * REPLAY MODE                                                        *
 **********************************************************************/

#define MAX_WARMUP (16384 * 100)
#define WARMUP_COLD 0
#define WARMUP_WORKLOAD 1
#define WARMUP_PAGETRACE 2
#define MAX_COLD (4096 * 64 * 4096)
#define REPLAY_PAST MAX_COLD

char cold[MAX_COLD];
static bool loaded = false;
static char *warmup[MAX_WARMUP];
static bool valid[MAX_WARMUP];
static int hotpages_counter = 0;

static int type_of_warmup = 0;

static char *get_filename_ext(const char *filename) {
  char *dot = strrchr(filename, '.');
  if (!dot || dot == filename)
    return "";
  return dot + 1;
}

static off64_t get_address_from_filename(char *filename) {
  off64_t address;
  sscanf(filename, "%lx.", &address);
  return address;
}

/* load: restores memory before replay
 * loop_name: name of dumped loop
 * invocation: invocation number to load
 * count: number of args
 * addresses: an array of <count> addresses
 */
void load(char *loop_name, int invocation, int count, void *addresses[count]) {
  char path[BUFSIZ];
  char buf[BUFSIZ + 1];

  /* XXX: If we replay a capture from a system with a different PAGESIZE, this
     may have unexpected side effects. We should save the PAGESIZE used during
     the capture */
  PAGESIZE = sysconf(_SC_PAGESIZE);

  char* dump_prefix = getenv("CERE_WORKING_PATH");
  if(!dump_prefix) {
    /* If CERE_WORKING_PATH is not defined fallback to default .cere */
    dump_prefix = ".cere";
  }

  /* Read warmup type from environment variable */
  char *ge = getenv("CERE_WARMUP");
  if (ge && strcmp("COLD", ge) == 0) {
    type_of_warmup = WARMUP_COLD;
  } else if (ge && strcmp("PAGETRACE", ge) == 0) {
    type_of_warmup = WARMUP_PAGETRACE;
  } else {
    /* WARMUP_WORKLOAD is the default warmup */
    type_of_warmup = WARMUP_WORKLOAD;
  }

  /* Load adresses */
  snprintf(path, sizeof(path), "%s/dumps/%s/%d/core.map", dump_prefix, loop_name,
           invocation);
  FILE *core_map = fopen(path, "r");
  if (!core_map)
    errx(EXIT_FAILURE, "Could not open %s", path);

  while (fgets(buf, BUFSIZ, core_map)) {
    int pos;
    off64_t address;
    sscanf(buf, "%d %lx", &pos, &address);
    addresses[pos] = (char *)(address);
  }
  fclose(core_map);

  if (!loaded) {
    /* load hotpages adresses */
    snprintf(path, sizeof(path), "%s/dumps/%s/%d/hotpages.map", dump_prefix, loop_name,
             invocation);
    FILE *hot_map = fopen(path, "r");
    if (!hot_map)
      warn("REPLAY: Could not open %s", path);

    while (fgets(buf, BUFSIZ, core_map)) {
      off64_t address;
      sscanf(buf, "%lx", &address);
      if (address == 0)
        continue;
      warmup[hotpages_counter++] = (char *)address;
      if (hotpages_counter >= MAX_WARMUP)
        break;
    }
    fclose(hot_map);

    loaded = true;
  }

  // No warmup at all for COLD.
  // COLD warmup must always be used with CERE_REPLAY_REPETITIONS=1
  if (type_of_warmup == WARMUP_COLD) {
    cacheflush();
    return;
  }

  DIR *dir;
  struct dirent *ent;
  struct stat st;
  int fp;
  int len;
  char filename[1024];
  int total_readed_bytes = 0;

  snprintf(path, sizeof(path), "%s/dumps/%s/%d/", dump_prefix, loop_name, invocation);
  if ((dir = opendir(path)) == NULL) {
    /* could not open directory */
    fprintf(stderr, "REPLAY: Could not open %s", path);
    exit(EXIT_FAILURE);
  }

  while ((ent = readdir(dir)) != NULL) {
    /* Read *.memdump files */
    if (strcmp(get_filename_ext(ent->d_name), "memdump") != 0)
      continue;

    snprintf(filename, sizeof(filename), "%s/%s", path, ent->d_name);
    if (stat(filename, &st) != 0) {
      fprintf(stderr, "Could not get size for file %s: %s\n", filename,
              strerror(errno));
      exit(EXIT_FAILURE);
    }

    fp = open(filename, O_RDONLY);
    if (fp < 0) {
      fprintf(stderr, "Could not open %s\n", filename);
      exit(EXIT_FAILURE);
    }

    off64_t address = get_address_from_filename(ent->d_name);

    int read_bytes = 0;
    while ((len = read(fp, (char *)(address + read_bytes), PAGESIZE)) > 0) {
      read_bytes += len;
    }

    // XXX This code should be only done once in the !loaded section
    // because the valid status does not depend on the repetition
    for (int i = 0; i < hotpages_counter; i++) {
      if ((warmup[i] >= (char *)address) &&
          (warmup[i] < ((char *)address + read_bytes))) {
        valid[i] = true;
        // printf("warmup = %p\n", warmup[i]);
      }
    }

    close(fp);
  }

  closedir(dir);

  /* No flush or warmup for WORKLOAD warmup */
  if (type_of_warmup == WARMUP_WORKLOAD)
    return;

  /* Flush and prepare TRACE warmup */
  /* invalid address should warm cold zone */
  int start = MAX(0, hotpages_counter - REPLAY_PAST);
  for (int i = start; i < hotpages_counter; i++) {
    if (!valid[i]) {
      warmup[i] =
          &cold[((unsigned long long)warmup[i]) % (MAX_COLD - 1)];
    }
  }

  /* Flush cache */
  cacheflush();

  /* Warmup cache */
  for (int i = start; i < hotpages_counter; i++) {
    for (char *j = warmup[i]; j < warmup[i] + PAGESIZE; j++)
      *j -= 1;
  }
  for (int i = start; i < hotpages_counter; i++) {
    for (char *j = warmup[i]; j < warmup[i] + PAGESIZE; j++)
      *j += 1;
  }
  int s = 0;
  for (int i = start; i < hotpages_counter; i++) {
    for (char *j = warmup[i]; j < warmup[i] + PAGESIZE; j++)
      s += *j;
  }
  for (int i = start; i < hotpages_counter; i++) {
    for (char *j = warmup[i]; j < warmup[i] + PAGESIZE; j++)
      __builtin_prefetch(j);
  }
}
