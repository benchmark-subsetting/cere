#pragma once

#include <sys/types.h>
#include <stdbool.h>

// LOCK

void* get_protect_offset(char* executable);
void tracer_lock_mem(pid_t pid);


// UNLOCK

bool should_unprotect(void ** start, void ** end, long long unsigned int * permissions);
void tracer_unlock_mem(pid_t pid);
