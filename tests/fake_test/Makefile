all: test
dump: test_dump
replay: test_replay

FLAGS=-g

test: dump.o test.c
	clang $(FLAGS) -c test.c -o test.o
	clang test.o dump.o -o test

test_dump: dump.o test.c
	clang $(FLAGS) -S -emit-llvm test.c -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-extractor-all.so -loop-extract-all test.ll -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-manager.so -loop-dump test.ll -o test.ll
	llc -march=x86-64 test.ll -o test.s
	clang test.s dump.o -o test_dump


LOOP=__extracted__checksum_for.cond
DUMP_FILES=$(wildcard dump/$(LOOP)/*.memdump)
SEGMENTS=$(DUMP_FILES:%.memdump=%)

test_replay: dump.o test.c
	clang $(FLAGS) -S -emit-llvm test.c -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-extractor-all.so -loop-extract-all test.ll -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-manager.so -loop-replay test.ll -o test.ll
	sed -i "/call.*run__extracted__init_for.cond/d" test.ll
	llc -march=x86-64 test.ll -o test.s
	echo ./generate_dump_objects.sh $(SEGMENTS)
	$(eval LDARGS := $(shell ./generate_dump_objects.sh $(SEGMENTS)))
	gcc -c dump/$(LOOP)/*.S
	gcc $(FLAGS) -o test_replay $(LDARGS) -Wl,--section-start=.init_array=0x800000 *.o test.s -lc

dump.o: dump.c
	clang $(FLAGS) -c dump.c

clean:
	- rm -f test test_* test.o test.s test.ll

veryclean: clean
	- rm -rf dump
	- rm -f dump.o
