SHELL=/bin/sh
BENCHMARK=is
BENCHMARKU=IS
TOP=../../..

include ../config/make.def

include ../sys/make.common

OBJS = is.s \
       ${COMMON}/c_print_results.o \
       ${COMMON}/c_timers.o \
       ${COMMON}/c_wtime.o \
       my_lib.o


${PROGRAM}: config ${OBJS}
	${CLINK} ${CLINKFLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

.c.o:
	${CCOMPILE} $<

is.o: is.c npbparams.h

is.s: npbparams.h
	$(CC) -S -emit-llvm is.c -o is.ll
	opt -S -loop-extract is.ll -o is.ll
	opt -S -load $(TOP)/../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-instrumentation.so -loop-instrumentation is.ll -o is.ll
	llc -march=x86-64 is.ll -o is.s

my_lib.o:
	make -C $(TOP)/external_lib_example/
	cp $(TOP)/external_lib_example/my_lib.o .

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f npbparams.h core
