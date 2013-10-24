all: test
dump: test_dump
replay: test_replay

FLAGS=-g -fPIC

test: dump.o test.c
	../../src/lec/lec test.c
	../../src/lel/lel test.o dump.o -o test

test_dump: dump.o test.c
	../../src/lec/lec --dump test.c $(FLAGS)
	../../src/lel/lel --dump$(FLAGS) test.o dump.o -o test_dump 

LOOP=__extracted__test_checksum_9_9

test_replay: dump.o  test.c
	../../src/lec/lec --replay=$(LOOP) test.c $(FLAGS)
	../../src/lel/lel --replay=$(LOOP) $(FLAGS) -o test_replay test.o dump.o -lc

dump.o: dump.c
	clang $(FLAGS) -c dump.c

clean:
	- rm -f *.o *.s *.ll

veryclean: clean
	- rm -rf dump
	- rm -f *.o test test_*
