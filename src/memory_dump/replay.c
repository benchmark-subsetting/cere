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

/**********************************************************************
 * REPLAY MODE                                                        *
 **********************************************************************/

#define MAX_WARMUP (16384 * 100)
#define MAX_COLD (4096 * 64)
#define REPLAY_PAST MAX_COLD

char cold[MAX_COLD * PAGESIZE];
extern char *cacheflush();
static bool loaded = false;
static char *warmup[MAX_WARMUP];
static bool valid[MAX_WARMUP];
static int hotpages_counter = 0;

static int type_of_warmup = 0;

char *get_filename_ext(const char *filename) {
  char *dot = strrchr(filename, '.');
  if (!dot || dot == filename)
    return "";
  return dot + 1;
}

char *get_filename_without_ext(char *filename) {
  char *dot = strrchr(filename, '.');
  if (!dot || dot == filename)
    return filename;
  int retstrlen = strlen(filename) - strlen(dot);
  char *retstr = malloc(retstrlen + 1);
  strncpy(retstr, filename, retstrlen);
  return retstr;
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

  char *ge = getenv("WARMUP_TYPE");
  if (ge) {
    type_of_warmup = atoi(ge);
    assert(type_of_warmup >= 0 && type_of_warmup < 3);
  }

  /* Load adresses */
  snprintf(path, sizeof(path), ".cere/dumps/%s/%d/core.map", loop_name,
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
    snprintf(path, sizeof(path), ".cere/dumps/%s/%d/hotpages.map", loop_name,
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

  // No warmup at all for NOWARMUP
  if (type_of_warmup == 2) {
    cacheflush();
    return;
  }

  DIR *dir;
  struct dirent *ent;
  struct stat st;
  int fp;
  char line[PAGESIZE];
  int len;
  char filename[1024];
  int total_readed_bytes = 0;

  snprintf(path, sizeof(path), ".cere/dumps/%s/%d/", loop_name, invocation);
  if ((dir = opendir(path)) == NULL) {
    /* could not open directory */
    fprintf(stderr, "REPLAY: Could not open %s", path);
    exit(EXIT_FAILURE);
  }

  while ((ent = readdir(dir)) != NULL) {
    /* Read *.memdump files */
    if (strcmp(get_filename_ext(ent->d_name), "memdump") != 0)
      continue;

    snprintf(filename, sizeof(filename), ".cere/dumps/%s/%d/%s", loop_name,
             invocation, ent->d_name);
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

    off64_t address;
    sscanf(get_filename_without_ext(ent->d_name), "%lx", &address);

    int read_bytes = 0;
    while ((len = read(fp, (char *)(address + read_bytes), PAGESIZE)) > 0) {
      read_bytes += len;
    }

    for (int i = 0; i < hotpages_counter; i++) {
      if ((warmup[i] >= (char *)address) &&
          (warmup[i] < ((char *)address + read_bytes))) {
        valid[i] = true;
        //printf("warmup = %p\n", warmup[i]);
      }
    }

    close(fp);
  }

  closedir(dir);

  /* No flush or warmup for OPTIMISTIC warmup */
  if (type_of_warmup == 1)
    return;

  /* invalid address should warm cold zone */
  int start = MAX(0, hotpages_counter - REPLAY_PAST);
  for (int i = start; i < hotpages_counter; i++) {
    if (!valid[i]) {
      warmup[i] =
          &cold[((unsigned long long) warmup[i]) % ((MAX_COLD - 1) * PAGESIZE)];
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
