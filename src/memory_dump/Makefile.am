lib_LTLIBRARIES = libcere_dump.la libcere_load.la
libcere_dump_la_CFLAGS = --std=gnu99 -I ../ccan/ -Wl,-z,now,-z,relro
libcere_dump_la_SOURCES = hooks.c dump.c counters.c ../ccan/ccan/htable/htable.c

libcere_load_la_CFLAGS = --std=gnu99 -Wl,-z,now,-z,relro
libcere_load_la_SOURCES = replay.c
