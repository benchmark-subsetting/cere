#ifndef __INTERFACE__H
#define __INTERFACE__H

#include "inject_syscall.h"

#define _DEBUG 1 
#undef _DEBUG

void protect(pid_t pid, char* start, char* end) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE PROTECTED :  %p\n", start);
#endif

  injection_code(PROTECT, pid, 2, start, end);
}

void unprotect(pid_t pid, char* start, char *end) {
#ifdef _DEBUG
  fprintf(stderr, "TO BE UNPROTECTED :  %p\n", start);
#endif

  injection_code(UNPROTECT, pid, 2, start, end);
}

void fake_syscall(void *arg) {
  syscall(SYS_fake, arg);
}

void sigtrap(void) {
  sigtrap_arch();
}

#endif /* INTERFACE__H */
