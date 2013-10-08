FLAGS=-g

DUMP_FILES=$(wildcard *.memdump)
SEGMENTS=$(DUMP_FILES:%.memdump=%)

all: original

patched: test.o dump.o $(DUMP_FILES)
	$(eval LDARGS := $(shell ./generate_dump_objects.sh $(SEGMENTS)))
	gcc -c *.S
	gcc -o patched $(LDARGS) -Wl,--section-start=.init_array=0x800000 *.o -lc

original: test.o dump.o
	gcc -o $@ $^

test.o: test.c
	clang $(FLAGS) -c test.c 

dump.o: dump.c
	clang $(FLAGS) -c dump.c 

clean:
	rm -f test dump.o *.S *.memdump *.o core.map original patched
