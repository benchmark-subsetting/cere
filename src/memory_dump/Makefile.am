lib_LTLIBRARIES = libcere_syscall.la libcere_dump.la libcere_load.la 

libcere_dump_la_CFLAGS = --std=gnu99 -Wl,-z,now
libcere_dump_la_SOURCES = hooks.c dump.c ptrace.c dump.h pages.h syscall_interface.h ptrace.h tracer.h tracer.c 
libcere_dump_la_LIBADD = libcere_syscall.la 

libcere_syscall_la_LDFLAGS = -fPIC -fomit-frame-pointer
licere_syscall_la_CFLAGS = --std=gnu99 -Wl,-z,now 
libcere_syscall_la_SOURCES = syscall_interface.c syscall_interface.h

libcere_load_la_CFLAGS = --std=gnu99 -Wl,-z,now
libcere_load_la_SOURCES = replay.c pages.h

bin_PROGRAMS = tracer
tracer_CFLAGS = --std=gnu99
tracer_SOURCES = ptrace.c tracer.c pages.h syscall_interface.h ptrace.h 
tracer_LDADD = libcere_syscall.la

