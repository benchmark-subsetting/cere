lib_LTLIBRARIES = libcere_dump.la libcere_load.la
libcere_dump_la_CFLAGS = --std=gnu99 -I ../ccan/ -Wl,-z,now
libcere_dump_la_SOURCES = hooks.c dump.c counters.c syscall_interface.c syscall_interface.h counters.h dump.h pages.h syscall_interface.h
libcere_dump_la_LIBADD = ../ccan/libccan.la

libcere_load_la_CFLAGS = --std=gnu99 -Wl,-z,now
libcere_load_la_SOURCES = replay.c pages.h

bin_PROGRAMS = tracer
tracer_CFLAGS = --std=gnu99 -I ../ccan/ -Wl,-z,now
tracer_SOURCES = ptrace.c tracer.c counters.c syscall_interface.c pages.h syscall_interface.h ptrace.hcounters.h dump.h ../ccan/libccan.la
tracer_LDADD = ../ccan/libccan.la

