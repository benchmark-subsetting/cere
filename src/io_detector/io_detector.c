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
//******************************io-detector.c*********************************//
// This library generates for each region an IO trace using strace. These     //
// traces are then parsed to detect IOs. Regions doing IOs are not replayable //
//****************************************************************************//

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <assert.h>
#include "io_detector.h"

// Comparison function.
static bool streq(const void *e, void *string)
{
  return strcmp(((region *)e)->name, string) == 0;
}

static uint32_t hash_string(const char *string)
{
  uint32_t ret;
  for (ret = 0; *string; string++)
    ret = (ret << 5) - ret + *string;
  return ret;
}

static size_t rehash(const void *e, void *unused)
{
  return hash_string(((region *)e)->name);
}

char** str_split(char* a_str, const char a_delim, int *count)
{
  char** result    = 0;
  char* tmp        = a_str;
  char* last_comma = 0;
  char delim[2];
  delim[0] = a_delim;
  delim[1] = 0;

  /* Count how many elements will be extracted. */
  while (*tmp)
  {
    if (a_delim == *tmp)
    {
      (*count)++;
      last_comma = tmp;
    }
    tmp++;
  }

  /* Add space for trailing token. */
  (*count) += last_comma < (a_str + strlen(a_str) - 1);

  /* Add space for terminating null string so caller
     knows where the list of returned strings ends. */
  (*count)++;

  result = malloc(sizeof(char*) * (*count));

  if (result)
  {
    size_t idx  = 0;
    char* token = strtok(a_str, delim);

    while (token)
    {
      assert(idx < (*count));
      *(result + idx++) = strdup(token);
      token = strtok(0, delim);
    }
    assert(idx == (*count) - 1);
    *(result + idx) = 0;
  }

  return result;
}

void create_directory(region *r, int invocation) {
  // Create the region directory
  char region_dir[PATH_MAX];
  snprintf(region_dir, sizeof(region_dir), ".cere/io_traces/%s", r->name);
  if (mkdir(region_dir, 0777) != 0)
    if (errno != EEXIST) errx(EXIT_FAILURE, "Iocheck: Could not create %s\n", region_dir);
  // Create the invocation directory
  char trace_dir[PATH_MAX];
  snprintf(trace_dir, sizeof(trace_dir), "%s/%d", region_dir, invocation);
  if (mkdir(trace_dir, 0777) != 0) {
    if (errno != EEXIST) errx(EXIT_FAILURE, "Iocheck: Could not create %s\n", trace_dir);
  }
}

void init_regions(char* filename) {
  char * line = NULL;
  size_t len = 0;
  char** tokens;
  int count = 0;

  FILE *input = fopen(filename, "r");
  if (!input)
    errx(EXIT_FAILURE, "Iochack: Unable to open the region file %s\n", filename);
  while(getline(&line, &len, input) != -1) {
    //Remove new line character
    strtok(line, "\n");
    count = 0;
    tokens = str_split(line, ' ', &count);
    if (tokens)
    {
      if(strlen(tokens[0]) > max_str_size)
        max_str_size = strlen(tokens[0]);
      region *r=NULL;
      if ((r = malloc(sizeof(region))) == NULL)
        errx(EXIT_FAILURE, "Iocheck: Unable to allocate new region %s\n", tokens[0]);
      if ((r->name = malloc((strlen(tokens[0])+1)*sizeof(char))) == NULL)
        errx(EXIT_FAILURE, "Iocheck: Unable to allocate new region name %s\n", tokens[0]);
      if ((r->requested_invocations = malloc((count-2)*sizeof(int))) == NULL)
        errx(EXIT_FAILURE, "Iocheck: Unable to allocate requested invocations for %s\n", tokens[0]);
      strcpy(r->name, tokens[0]);
      r->nb_invoc = count-2;
      r->current_invocation = 0;
      int i;
      for (i = 1; tokens[i]; i++)
      {
        r->requested_invocations[i-1] = atoi(tokens[i]);
        create_directory(r, r->requested_invocations[i-1]);
        free(tokens[i]);
      }
      free(tokens);
      htable_add(&regionHtab, hash_string(r->name), r);
    }
  }
}

bool is_requested_invoc(region *r) {
  int i;
  for (i = 0; i < r->nb_invoc; i++)
  {
    if (r->current_invocation == r->requested_invocations[i])
      return true;
  }
  return false;
}

void init_io_detection(char* filename) {
  htable_init(&regionHtab, rehash, NULL);
  init_regions(filename);
}

void start_io_detection(char* regName) {
  region *r=NULL;
  // Search region in the hash table
  if ((r = htable_get(&regionHtab, hash_string(regName), streq, regName)) == NULL)
  {
    fprintf(stderr, "Iocheck: Starting a not initialized region %s. I Can't trace it\n", regName);
    return;
  }
  r->current_invocation++;
  if(!is_requested_invoc(r)) {
    return;
  }
  // Set trace file name
  char trace_file[PATH_MAX];
  snprintf(trace_file, sizeof(trace_file), ".cere/io_traces/%s/%d/%s",
           r->name, r->current_invocation, r->name);
  // Remove previous trace
  if (unlink(trace_file) < 0) {
    if (errno != ENOENT) perror("Iocheck: Cannot delete previous trace\n");
  }
  if(tracing) {
    // We are already tracing from a top level region.
    fprintf(stderr, "CERE_START %s %d\n", r->name, r->current_invocation);
    return;
  }

  pid_t pid = fork();
  if(pid < 0) perror("Iocheck: fork failed\n");
  if (pid != 0) {
    char command[BUF_SIZE];
    char str_size[BUF_SIZE];
    snprintf(command, BUF_SIZE, "%d", pid);
    snprintf(str_size, BUF_SIZE, "%u", max_str_size+40);

    // -s defined the line output size of strace to str_size which is the
    // longest region name.
    char *args[] = {
      "/usr/bin/strace",
      "-s",
      str_size,
      "-p",
      command,
      "-e",
      "trace=read,write,mmap",
      "-o",
      trace_file,
      NULL
    };
    // Run strace
    execvp(args[0], args);
    errx(EXIT_FAILURE, "Iocheck: could not execute strace\n");
  }

  // we are in the child process, let's wait for strace to start tracing before
  // pursuing the application execution
  tracing = getppid();
  r->pid = getppid();
  r->strace = true;
  sleep(0.1);
  int f;
  while((f = open(trace_file, O_RDONLY )) == -1) sleep(0.1);
  close(f);
}

void stop_io_detection(char* regName) {
  region *r=NULL;
  if ((r = htable_get(&regionHtab, hash_string(regName), streq, regName)) == NULL)
    fprintf(stderr, "Iocheck: Unable to find the markerStart for region >%s<\n", regName);
  else if(is_requested_invoc(r)) {
    if(r->strace) {
      //Kill strace
      if (kill(r->pid, SIGTERM) < 0) perror("Iocheck: kill strace failed");
      tracing = 0;
    }
    else fprintf(stderr, "CERE_STOP %s %d\n", r->name, r->current_invocation);
  }
}

void close_io_detection() {
  htable_clear(&regionHtab);
}
