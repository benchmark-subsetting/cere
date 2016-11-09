lib_LTLIBRARIES = libcere_dump.la libcere_load.la 

libcere_dump_la_LDFLAGS = -fPIC -ldl
libcere_dump_la_CFLAGS = -g --std=gnu99
libcere_dump_la_SOURCES = tracee.c tracee_hooks.c tracee_interface.c tracee.h tracee_interface.h pages.h types.h err.h

libcere_load_la_CFLAGS = -g --std=gnu99 -Wl,-z,now
libcere_load_la_SOURCES = replay.c pages.h

bin_PROGRAMS = cere-tracer
cere_tracer_CFLAGS = -I ../ccan/ -g --std=gnu99 -Wl,-z,now
cere_tracer_SOURCES = ptrace.c cere_tracer.c tracer_interface.c pages.h tracer_interface.h ptrace.h types.h err.h
cere_tracer_LDADD = ../ccan/libccan.la
