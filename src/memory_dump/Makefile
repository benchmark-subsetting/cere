CFLAGS=-g
CC=clang

LOOP=mainloop
DUMP_FILES=$(wildcard dump/$(LOOP)/1/*.memdump)
SEGMENTS=$(DUMP_FILES:%.memdump=%)

all: dump.a malloc_hook.so

patched: test.o dump.a $(DUMP_FILES)
	$(eval LDARGS := $(shell ./generate_dump_objects.sh $(SEGMENTS)))
	gcc -c dump/$(LOOP)/1/*.S
	gcc $(CFLAGS) -o patched $(LDARGS) -Wl,--section-start=.init_array=0x800000 *.o dump.a -lc

original: test.o dump.a
	gcc $(CFLAGS) -o $@ $^


htable.o: ../htable/htable/htable.c
	$(CC) $(CFLAGS) -c ../htable/htable/htable.c

dump.a: dump.o htable.o snoop.o
	ar -r dump.a htable.o dump.o snoop.o

malloc_hook.o: malloc_hook.c
	clang -fPIC -o $@ -c $<

malloc_hook.so: malloc_hook.o
	clang -shared -ldl -o $@ $<

clean:
	rm -f test test.c dump.a *.S *.memdump *.o core.map original patched
	rm -rf dump/
