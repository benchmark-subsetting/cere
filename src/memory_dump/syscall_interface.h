#ifndef __SYSCALL_INTERFACE__H
#define __SYSCALL_INTERFACE__H

#define SYS_fake (-1)

typedef enum {PROTECT, UNPROTECT} syscallID;

void protect(pid_t pid, char *start, char *end);
void unprotect(pid_t pid, char *start, char *end);
void fake_syscall(void *arg);

void* get_arg_fake_syscall(pid_t pid);
void get_string(pid_t pid, char *loop, size_t nbyte);
void sigtrap(void);
void set_var(pid_t pid, void* arg, void* val);

#endif /* __SYSCALL_INTERFACE__H */ 
