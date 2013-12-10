PROJECT_ROOT=../../

all: test
dump: test_dump
replay: test_replay

FLAGS=-g

test: test.c
	../../src/lec/lec $(FLAGS) test.c
	../../src/lel/lel $(FLAGS) test.o -o test

test_dump: test.c
	../../src/lec/lec --dump $(FLAGS) test.c
	../../src/lel/lel --dump $(FLAGS) test.o -o test_dump

LOOP=__extracted__test_checksum_7

test_replay: test.c
	../../src/lec/lec --replay=$(LOOP) $(FLAGS) test.c
	../../src/lel/lel --replay=$(LOOP) $(FLAGS) -o test_replay test.o -lc

#~ dump.o:
	#~ cp $(PROJECT_ROOT)/src/memory_dump/dump.c .
	#~ clang $(FLAGS) -c dump.c

clean:
	- rm -f *.o *.s *.ll

veryclean: clean
	- rm -rf dump/
	- rm -f *.o test test_* lel_bin realmain.c dump.c
