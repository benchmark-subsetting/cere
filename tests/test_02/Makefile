SHELL=/bin/sh
BENCHMARK=is
BENCHMARKU=IS
PROJECT_ROOT=../../

include make.def


PROGRAM=IS

OBJS = is.s \
       c_print_results.o \
       c_timers.o \
       wtime.o \
       my_lib.o


${PROGRAM}: ${OBJS}
	${CLINK} ${CLINKFLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

.c.o:
	${CC} -c $<

is.o: is.c npbparams.h

is.s: npbparams.h
	$(CC) -S -emit-llvm is.c -o is.ll
	opt -S -loop-extract is.ll -o is.ll
	opt -S -load $(PROJECT_ROOT)/src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-instrumentation.so -loop-instrumentation is.ll -o is.ll
	llc -march=x86-64 is.ll -o is.s

my_lib.o:
	make -C $(PROJECT_ROOT)/tests/external_lib_example/
	cp $(PROJECT_ROOT)/tests/external_lib_example/my_lib.o .

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f IS core
