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

// As this file will be compiled as an alternative, baremetal version of the
// standard load library, we keep all functions and their names, only
// modifying what's need for baremetal.

/*
 * Extern references to embedded symbols
 */

// memchunks_sizes
extern char _binary_memchunks_sizes_start[];
extern char _binary_memchunks_sizes_end[];
extern char _binary_memchunks_sizes_size[];

// memchunks_addressses
extern char _binary_memchunks_addresses_start[];
extern char _binary_memchunks_addresses_end[];
extern char _binary_memchunks_addresses_size[];

// concatenated_chunks
extern char _binary_concatenated_memchunks_start[];
extern char _binary_concatenated_memchunks_end[];
extern char _binary_concatenated_memchunks_size[];

// core
extern char _binary_core_map_start[];
extern char _binary_core_map_end[];
extern char _binary_core_map_size[];

// hotpages
extern char _binary_hotpages_map_start[];
extern char _binary_hotpages_map_end[];
extern char _binary_hotpages_map_size[];


#define _LARGEFILE64_SOURCE
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
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


// Dummy replacer because this will be compiled in -nostdlib
void __stack_chk_fail(void) {}
void __unnamed_1(void) {}

int real_main(void);
void _start(char *args, ...) {
  real_main();
}



void cacheflush(void) {
  const size_t size = CACHE_SIZE_MB * 1024 * 1024;
  int i, j;
  char c[CACHE_SIZE_MB * 1024 * 1024];
  for (i = 0; i < 5; i++)
    for (j = 0; j < size; j++)
      c[j] = i * j;
}

/*
 * BAREMETAL HELPERS
 */

inline int get_decimal_digit(char d) {
  // Assumes d is in the '0' - '9' range
  return d - 48; // 48 -> '0'
}

inline int get_hex_digit(char d) {
  // Assumes d is in the '0' - '9' or 'a' - 'f' ranges
  if(d >= 48 && d <= 57) {
    return get_decimal_digit(d);
  }
  return d - 87; // 97 -> 'a', is worth 10
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


/*
 * BAREMETAL SPECIFIC FUNCTIONS
 */

void coremap_sscanf(char * buf, int * pos, off64_t * address) {
  /*
  *
  * Reads :
  * - pos integer in base 10
  * - address in base 16
  * Both are space separated
  */

  /* pos */
  int pos_ndigits = 0;
  while(buf[pos_ndigits] != ' ') {
    pos_ndigits++;
  }

  *pos = 0;
  // Read digit
  for(int i=0; i<=pos_ndigits-1; i++) {
    *pos = *pos * 10 + get_decimal_digit(buf[i]);
  }

  /* address */
  int base = pos_ndigits+1;
  int address_ndigits = 0;
  while(buf[base + address_ndigits] != '\n') {
    address_ndigits++;
  }

  *address = 0;
  for(int i=0; i<=address_ndigits-1; i++) {
    *address = *address * 16 + get_hex_digit(buf[base + i]);
  }
}

void load_core_map(int count, void *addresses[count]) {

  char buf[BUFSIZ + 1];

  /* Load core_map symbols */
  char *core_start = _binary_core_map_start;
  char *core_end = _binary_core_map_end;

  char* core = core_start;

  /* Read & process all core entries */
  while(core < core_end) {

    /* Read a single core entry */
    int i = 0;
    while(i < BUFSIZ / sizeof(char)) {
      buf[i] = *core;
      core += sizeof(char);
      i++;
      if(buf[i-1] == '\n')
        break;
    }

    // If we read the maximum size without encountering \n or reach the
    // end of core, add the \n ourselves
    if(i == BUFSIZ || core == core_end)
      buf[i] = '\n';

    /* Load the core entry just stored in buf */
    int pos;
    off64_t address;
    coremap_sscanf(buf, &pos, &address);
    addresses[pos] = (char *)(address);
  }

}


void hotpages_sscanf(char * buf, off64_t * address) {
  /* address */
  int base = 2; // Because buf starts with "0x"
  int address_ndigits = 0;
  while(buf[base + address_ndigits] != '\n') {
    address_ndigits++;
  }

  *address = 0;
  for(int i=0; i<=address_ndigits-1; i++) {
    *address = *address * 16 + get_hex_digit(buf[base + i]);
  }

}

void load_hotpages_map() {

  char buf[BUFSIZ + 1];

  /* Load hotpages_map symbols */
  char *hotpages_start = _binary_hotpages_map_start;
  char *hotpages_end = _binary_hotpages_map_end;

  char* hotpages = hotpages_start;

  /* Read & process all core entries */
  while(hotpages < hotpages_end) {

    /* Read a single hotpages entry */
    int i = 0;
    while(i < BUFSIZ / sizeof(char)) {
      buf[i] = *hotpages;
      hotpages += sizeof(char);
      i++;
      if(buf[i-1] == '\n')
        break;
      }

      // If we read the maximum size without encountering \n or reach the
      // end of core, add the \n ourselves
      if(i == BUFSIZ || hotpages == hotpages_end)
        buf[i] = '\n';

      /* Load the hotpages entry just stored in buf */
      off64_t address;
      hotpages_sscanf(buf, &address);
      if (address == 0)
        continue;
      warmup[hotpages_counter++] = (char *)address;
      if (hotpages_counter >= MAX_WARMUP)
        break;
    }

}

// Puts count bytes from the memchunk into the dest buf
ssize_t read_memchunk(unsigned char * memchunk_start, size_t memchunk_size, unsigned char ** current_memchunk, unsigned char *buf, size_t count) {

  // Error checking to mimic read's behaviour
  if(count == 0) {
    return 0;
  }
  if(*current_memchunk + count > memchunk_start + memchunk_size) {
    return 0;
  }

  for(size_t i=0; i<count; i++) {
    buf[i] = (*current_memchunk)[i];
  }

  // Change offset
  *current_memchunk += count;

  return count;
}

void load_memdumps() {

  int len;

  // memchunks_sizes
  int32_t* memchunks_sizes_start = (int32_t*) _binary_memchunks_sizes_start;
  int32_t* memchunks_sizes_end = (int32_t*) _binary_memchunks_sizes_end;

  // memchunks_addresses (we do not make any assumption on addresses sizes)
  unsigned char* memchunks_addresses_start = (unsigned char*) _binary_memchunks_addresses_start;
  unsigned char* memchunks_addresses_end = (unsigned char*) _binary_memchunks_addresses_end;

  // Address size is the size of the memchunks_address symbol on the number of memchunks
  size_t address_size = (memchunks_addresses_end - memchunks_addresses_start) / (memchunks_sizes_end - memchunks_sizes_start);

  // concatenated_chunks
  unsigned char* concatenated_memchunks_start = (unsigned char*) _binary_concatenated_memchunks_start;
  unsigned char* concatenated_memchunks_end = (unsigned char*) _binary_concatenated_memchunks_end;

  while(memchunks_sizes_start < memchunks_sizes_end) {

    // Get chunk size (assuming sizeof(int) == 4)
    int32_t memchunk_size = *memchunks_sizes_start;

    // Read chunk address byte by byte
    off64_t memchunk_address = 0;
    for(size_t i=0; i<address_size; i++) {
      unsigned char current_byte = (unsigned char) *memchunks_addresses_start;
      memchunk_address = (memchunk_address << 8) + (off64_t) current_byte;
      memchunks_addresses_start++;
    }

    // Get chunk content
    int read_bytes = 0;
    unsigned char * current_memchunk = concatenated_memchunks_start;
    while ((len = read_memchunk(concatenated_memchunks_start, memchunk_size, &current_memchunk, (unsigned char *)(memchunk_address + read_bytes), PAGESIZE)) > 0) {
      read_bytes += len;
    }

    // XXX This code should be only done once in the !loaded section
    // because the valid status does not depend on the repetition
    for (int i = 0; i < hotpages_counter; i++) {
      if ((warmup[i] >= (char *)memchunk_address) &&
          (warmup[i] < ((char *)memchunk_address + read_bytes))) {
        valid[i] = true;
      }
    }


    // Update chunks' size and content pointers
    memchunks_sizes_start++;
    concatenated_memchunks_start += memchunk_size;
  }
}


// MAIN LOAD FUNCTION
/* load: restores memory before replay
 * loop_name: name of dumped loop
 * invocation: invocation number to load
 * count: number of args
 * addresses: an array of <count> addresses
 */
void load(char *loop_name, int invocation, int count, void *addresses[count]) {
  char path[BUFSIZ];
  char buf[BUFSIZ + 1];

  // NOTE Assuming PAGESIZE of 4096
  PAGESIZE = 4096;
  // NOTE Assuming WARMUP_WORKLOAD in baremetal
  type_of_warmup = WARMUP_WORKLOAD;

  load_core_map(count, addresses);

  if (!loaded) {
    load_hotpages_map();
    loaded = true;
  }

  // No warmup at all for COLD.
  // COLD warmup must always be used with CERE_REPLAY_REPETITIONS=1
  if (type_of_warmup == WARMUP_COLD) {
    cacheflush();
    return;
  }

  load_memdumps();

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
