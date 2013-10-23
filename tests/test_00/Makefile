all: test
dump: test_dump
replay: test_replay

FLAGS=-g

test: dump.o test.c
	../../src/lec/lec test.c
	../../src/lel/lel test.o dump.o -o test

test_dump: dump.o test.c
	../../src/lec/lec --dump test.c
	../../src/lel/lel --dump test.o dump.o -o test_dump

LOOP=__extracted__checksum_for.cond

test_replay: dump.o test.c
	../../src/lec/lec --replay=$(LOOP) test.c
	../../src/lel/lel --replay=$(LOOP) $(FLAGS) -o test_replay test.o dump.o -lc

dump.o: dump.c
	clang $(FLAGS) -c dump.c

clean:
	- rm -f test test_* test.o test.s test.ll

veryclean: clean
	- rm -rf dump
	- rm -f dump.o
