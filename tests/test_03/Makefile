SHELL=/bin/sh
BENCHMARK=is
BENCHMARKU=IS
PROJECT_ROOT=../../

include make.def

CC=../../src/lec/lec
CLINK=../../src/lel/lel

FLAGS=-g

PROGRAM=IS

OBJS = is.o \
       c_print_results.o \
       c_timers.o \
       wtime.o


${PROGRAM}: ${OBJS}
	${CLINK} ${MODE} ${CLINKFLAGS} ${FLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

.c.o:
	${CC} ${MODE} ${FLAGS} $<

is.o: is.c npbparams.h

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f IS core

veryclean: clean
	- rm -rf cere_dumps/
	- rm -f lel_bin realmain.c dump.c
