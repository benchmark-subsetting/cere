#include <assert.h>
#include <fcntl.h>
#include <err.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <syscall.h>
#include <unistd.h>
#include <elf.h>
#include <sys/mman.h>


#define _DEBUG 1
#undef _DEBUG

#include "debug.h"

#include "ptrace.h"
#include "pages.h"
#include "tracer_interface.h"
#include "types.h"

long PAGESIZE;
void *addresses[65536];
long long unsigned int addrs_permissions[32768];
int addrs_counter = 0;

#if defined(__LP64__)
#define ElfW(type) Elf64_ ## type
#else
#define ElfW(type) Elf32_ ## type
#endif


/*****
* LOCK
******/


void* get_protect_offset(char* executable) {
  /* Returns the offset at which to start memory protection as to
   * avoid sections overlap errors in replay mode. */

  ElfW(Ehdr) *ehdr;
  ElfW(Shdr) *shdr;
  void *data;
  char *strtab;
  int filesize, file;

  file = open(executable, O_RDONLY);

  if(file != -1) {
    filesize = lseek(file, 0, SEEK_END);
    data = mmap(NULL, filesize, PROT_READ, MAP_SHARED, file, 0);
    if (data != NULL) {
      ehdr = (ElfW(Ehdr) *)(data);
      shdr = (ElfW(Shdr) *)(data + ehdr->e_shoff);
      strtab = (char *)(data + shdr[ehdr->e_shstrndx].sh_offset);

      for(int i=1; i < ehdr->e_shnum; i++) {
        char * name = &strtab[shdr[i].sh_name];
        void * address = (void*) shdr[i].sh_addr;

        if(strcmp(name, ".init") == 0) {
          // We will start to protect from this offset
          close(file);
          return address;
        }
      }

    } else {
      errx(EXIT_FAILURE, "ELF found but could not read its content correctly");
      return NULL;
    }
  } else {
    errx(EXIT_FAILURE, "Could not find ELF");
    return NULL;
  }


  errx(EXIT_FAILURE, "Could not find .init section in ELF");
  return NULL;
}

void get_permissions_from_buf(char * buf, long long unsigned int * permissions) {
  /* From a string in "rwx" format, return the corresponding encoded integer
   * ready to use with mprotect */

  long long unsigned int new_permissions;
  if(strstr(buf, "--x") != NULL) {
    new_permissions = (long long unsigned)(PROT_EXEC);
    memcpy(
      permissions,
      &new_permissions,
      sizeof(long long unsigned int)
    );
  }
  else if(strstr(buf, "-w-") != NULL) {
    new_permissions = (long long unsigned)(PROT_WRITE);
    memcpy(
      permissions,
      &new_permissions,
      sizeof(long long unsigned int)
    );
  }
  else if(strstr(buf, "-wx") != NULL) {
    new_permissions = (long long unsigned)(PROT_WRITE|PROT_EXEC);
    memcpy(
      permissions,
      &new_permissions,
      sizeof(long long unsigned int)
    );
  }
  else if(strstr(buf, "r--") != NULL) {
    new_permissions = (long long unsigned)(PROT_READ);
    memcpy(
      permissions,
      &new_permissions,
      sizeof(long long unsigned int)
    );
  }
  else if(strstr(buf, "r-x") != NULL) {
    new_permissions = (long long unsigned)(PROT_READ|PROT_EXEC);
    memcpy(
      permissions,
      &new_permissions,
      sizeof(long long unsigned int)
    );
  }
  else if(strstr(buf, "rw-") != NULL) {
    new_permissions = (long long unsigned)(PROT_READ|PROT_WRITE);
    memcpy(
      permissions,
      &new_permissions,
      sizeof(long long unsigned int)
    );
  }
  else if(strstr(buf, "rwx") != NULL) {
    new_permissions = (long long unsigned)(PROT_READ|PROT_WRITE|PROT_EXEC);
    memcpy(
      permissions,
      &new_permissions,
      sizeof(long long unsigned int)
    );
  }
}

void tracer_lock_mem(pid_t pid) {

  PAGESIZE = sysconf(_SC_PAGESIZE);

  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);
  FILE *maps = fopen(maps_path, "r");

  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  debug_print("%s\n", "START LOCK MEM");

  char buf[BUFSIZ + 1];
  bool first_pages = true;
  char *tmpstr = NULL;
  char *executable = NULL;
  void *protect_offset = NULL;
  long long unsigned int permissions;

  while (fgets(buf, BUFSIZ, maps)) {
    void *start, *end;

    // debug_print("%s", buf);
    sscanf(buf, "%p-%p", &start, &end);

    /* Ignore libc special mem zones  */
    /* We have to find a good criteria to detect that specific memory page, */
    /* for now we will consider that this will always be the first page. */
    /* If we don't ignore those mem zones we get this error */
    /* /usr/bin/ld: la section .interp chargée à  */
    /*    [00000000004003c0 -> 00000000004003db]  */
    /*    chevauche la section s000000400000 chargée à  */
    /*    [0000000000400000 -> 0000000000400fff] */
    if (first_pages) {
      first_pages = false;

      // We should be able to get the executable name from buf ...
      tmpstr = strchr(buf, '/');
      executable = malloc(strlen(tmpstr-1)*sizeof(char));
      memcpy(executable, tmpstr, strlen(tmpstr-1)*sizeof(char));
      executable[strlen(executable)-1] = 0;
      // ... and inspect the ELF to determine where to start protecting pages
      protect_offset = get_protect_offset(executable);
      if(protect_offset == NULL)
        exit(1);
      protect_offset = (void*) ((uint64_t) protect_offset + (uint64_t) getpagesize());
      debug_print("protect_offset=%p\n", protect_offset);
      free(executable);
    }

    if(start < protect_offset)
        continue;

    /* Ignore libdump mem zones  */
    if (strstr(buf, "libcere_dump.so") != NULL)
      continue;

    /* Ignore libc pages  */
    if (strstr(buf, "linux-gnu") != NULL)
      continue;

    /* Ignore vsyscall special mem zones  */
    if (strstr(buf, "vsyscall") != NULL)
      continue;

    /* Ignore libdump vdso zones  */
    if (strstr(buf, "vdso") != NULL)
      continue;

    /* Ignore vvar zone (cf. https://lkml.org/lkml/2015/3/12/602) */
    if (strstr(buf, "vvar") != NULL)
      continue;

    /* Ignore alreay protected pages */
    if (strstr(buf, "---p") != NULL) {
      // Already protected pages are a special case, as we don't want to unprotect
      // them after the codelet
      assert(addrs_counter < 65536);
      continue;
    }

    assert(addrs_counter < 65536);

    /* This will be protected : save adddresses/permissions to unprotect later */
    debug_print("round_to_page=%p\n", round_to_page(start));
    get_permissions_from_buf(&(buf[0]), addrs_permissions+addrs_counter);
    addresses[addrs_counter++] = round_to_page(start);
    addresses[addrs_counter++] = end;
  }

  /* Protect all pages in addresses */
  int tmp_counter = addrs_counter;
  while (tmp_counter > 0) {
    void *end = addresses[--tmp_counter];
    void *start = addresses[--tmp_counter];
    protect_i(pid, start, (end - start));
  }

  int r = fclose(maps);
  if (r != 0)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ %s\n",
         strerror(errno));

  debug_print("%s\n", "END LOCK MEM");
}



/*******
* UNLOCK
********/

inline bool should_unprotect(void ** start, void ** end, long long unsigned int * permissions) {
  /* For a memory range, check if part (or all) of it has been protected,
   * and retrieves its previous permissions. If it has been protected, return true. */

  // Iterate over previously protected address ranges to determine
  // if a given range should be unprotected
  int i = addrs_counter;
  while (i > 0) {
    void *i_end = addresses[--i]  ;
    void *i_start = addresses[--i];

    // Overlap between the two memory ranges ?
    if((*start < i_end) && (*end > i_start)) {
      // In case the memory ranges of the proc maps file have changed,
      // return the intersection of the two ranges
      *start = *start > i_start ? *start : i_start;
      *end = *end < i_end ? *end : i_end;

      // Also retrieve the previous permissions
      memcpy(permissions, addrs_permissions+((i-2)/2), sizeof(long long unsigned int));
      return true;
    }
  }

  // If range is in none of the previously protexted range,
  // we should not unprotect it
  return false;
}

void tracer_unlock_mem(pid_t pid) {

  char maps_path[MAX_PATH];
  sprintf(maps_path, "/proc/%d/maps", pid);
  FILE *maps = fopen(maps_path, "r");

  if (!maps)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ interface");

  debug_print("%s\n", "START UNLOCK MEM");

  void *unprotect_addresses[65536];
  long long unsigned int unprotect_addrs_permissions[32768];
  long long unsigned int current_permissions;

  char buf[BUFSIZ + 1];
  int counter = 0;

  while (fgets(buf, BUFSIZ, maps)) {
    void *start, *end;

    debug_print("%s", buf);
    sscanf(buf, "%p-%p", &start, &end);

    assert(counter < 65536);

    if(should_unprotect(&start, &end, &current_permissions)) {
      memcpy(unprotect_addrs_permissions+(counter/2), &current_permissions, sizeof(long long unsigned int));
      unprotect_addresses[counter++] = round_to_page(start);
      unprotect_addresses[counter++] = end;
    }

  }


  /* Unprotect all pages in adresses */
  int i = counter;
  while (i > 0) {
    void *end = unprotect_addresses[--i];
    void *start = unprotect_addresses[--i];
    unprotect_i(pid, start, (end - start));
  }

  int r = fclose(maps);
  if (r != 0)
    errx(EXIT_FAILURE, "Error reading the memory using /proc/ %s\n",
         strerror(errno));

  debug_print("%s\n", "END UNLOCK MEM");
}
