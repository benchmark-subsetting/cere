FLAGS=-g

LOOP=mainloop
DUMP_FILES=$(wildcard dump/$(LOOP)/1/*.memdump)
SEGMENTS=$(DUMP_FILES:%.memdump=%)

all: original

patched: test.o dump.a $(DUMP_FILES)
	$(eval LDARGS := $(shell ./generate_dump_objects.sh $(SEGMENTS)))
	gcc -c dump/$(LOOP)/1/*.S
	gcc $(FLAGS) -o patched $(LDARGS) -Wl,--section-start=.init_array=0x800000 *.o dump.a -lc

original: test.o dump.a
	gcc $(FLAGS) -o $@ $^

test.o: test.c
	clang $(FLAGS) -c test.c

dump.a: dump.c 
	clang $(FLAGS) -c dump.c
	clang $(FLAGS) -c ../htable/htable/htable.c
	ar -r dump.a htable.o dump.o

clean:
	rm -f test test.c dump.a *.S *.memdump *.o core.map original patched 
	rm -rf dump/
