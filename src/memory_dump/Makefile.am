lib_LTLIBRARIES = libdump.la
libdump_la_CFLAGS = --std=gnu99 -I ../ccan/
libdump_la_SOURCES = mallochook.c dump.c counters.c ../ccan/ccan/htable/htable.c

noinst_LIBRARIES = libload.a
libload_a_CFLAGS = --std=gnu99
libload_a_SOURCES = replay.c warm.c
