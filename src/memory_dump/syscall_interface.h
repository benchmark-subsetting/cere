#ifndef __SYSCALL_INTERFACE__H
#define __SYSCALL_INTERFACE__H

#include <tracer.h>

/* #define sigtrap asm volatile ("int $3") */

typedef enum {PROTECT, UNPROTECT, UNPROTECT_STATE, WRITE, OPENAT, CLOSE} syscallID;

void protect(pid_t pid, char *start, size_t size);
void unprotect(pid_t pid, char *start, size_t size);
void unprotect_state(pid_t pid, char *start, size_t size);
void write_page(pid_t pid, int fd, const void* buf, size_t nbyte);
int openat_i(pid_t pid, char *pathname);
void close_i(pid_t pid, int fd);

register_t get_arg_from_regs(pid_t pid);

void send_to_tracer(register_t arg);

void get_string(pid_t pid, char *src_tracee, char *dst_tracer, size_t nbyte);
void put_string(pid_t pid, char *src, void *dst, size_t nbyte);

void sigtrap(void);

#endif /* __SYSCALL_INTERFACE__H */ 

