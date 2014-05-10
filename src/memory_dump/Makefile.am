lib_LTLIBRARIES = libdump.la
libdump_la_CFLAGS = --std=gnu99
libdump_la_SOURCES = mallochook.c dump.c counters.c ../ccan/ccan/htable/htable.o

noinst_LIBRARIES = libload.a
libload_a_CFLAGS = --std=gnu99
libload_a_SOURCES = replay.c warm.c
