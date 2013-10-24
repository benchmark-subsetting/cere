SHELL=/bin/sh
BENCHMARK=is
BENCHMARKU=IS
PROJECT_ROOT=../../

include make.def

CC=../../src/lec/lec
CLINK=../../src/lel/lel

PROGRAM=IS

OBJS = is.o \
       c_print_results.o \
       c_timers.o \
       wtime.o \
	   dump.o 


${PROGRAM}: ${OBJS}
	${CLINK} ${MODE} ${CLINKFLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

.c.o:
	${CC} ${MODE} $<

dump.o:
	cp $(PROJECT_ROOT)/src/memory_dump/dump.c .
	clang -c dump.c

is.o: is.c npbparams.h

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f IS core
veryclean: clean
	- rm -rf dump
