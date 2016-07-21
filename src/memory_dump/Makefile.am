lib_LTLIBRARIES = libcere_dump.la libcere_load.la 

libcere_dump_la_LDFLAGS = -fPIC -ldl
libcere_dump_la_CFLAGS = --std=gnu99
libcere_dump_la_SOURCES = hooks.c dump.c ptrace.c cere_tracer.c dump.h pages.h syscall_interface.c syscall_interface.h ptrace.h types.h

libcere_load_la_CFLAGS = --std=gnu99 -Wl,-z,now
libcere_load_la_SOURCES = replay.c pages.h

bin_PROGRAMS = cere-tracer
cere_tracer_CFLAGS = --std=gnu99 -Wl,-z,now
cere_tracer_SOURCES = ptrace.c cere_tracer.c syscall_interface.c pages.h syscall_interface.h ptrace.h types.h
cere_tracer_LDADD = libcere_dump.la
