lib_LTLIBRARIES = libdump.la libload.la
libdump_la_CFLAGS = --std=gnu99 -I ../ccan/
libdump_la_SOURCES = hooks.c dump.c counters.c ../ccan/ccan/htable/htable.c

libload_la_CFLAGS = --std=gnu99
libload_la_SOURCES = replay.c warm.c
