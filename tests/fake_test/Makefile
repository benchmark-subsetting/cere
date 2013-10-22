all: test
dump: test_dump
replay: test_replay

test: dump.o test.c
	clang -c test.c -o test.o
	clang test.o dump.o -o test

test_dump: dump.o test.c
	clang -S -emit-llvm test.c -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-extractor-all.so -loop-extract-all test.ll -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-manager.so -loop-dump test.ll -o test.ll
	llc -march=x86-64 test.ll -o test.s
	clang test.s dump.o -o test_dump

test_replay: dump.o test.c
	clang -S -emit-llvm test.c -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-extractor-all.so -loop-extract-all test.ll -o test.ll
	opt -S -load ../../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-manager.so -loop-replay test.ll -o test.ll
	llc -march=x86-64 test.ll -o test.s
	clang test.s dump.o -o test_replay

dump.o: dump.c
	clang -c dump.c

clean:
	- rm -f test test_* test.o test.s test.ll

veryclean: clean
	- rm -rf dump
	- rm -f dump.o
