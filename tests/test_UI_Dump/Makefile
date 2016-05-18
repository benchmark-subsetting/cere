SHELL=/bin/sh
BENCHMARK=is
BENCHMARKU=IS
PROJECT_ROOT=../../

include make.def

CC=../../src/cerec
CLINK=../../src/cerec

FLAGS=-g

PROGRAM=IS

OBJS = is.o \
       c_print_results.o \
       c_timers.o \
       wtime.o


${PROGRAM}: ${OBJS}
	${CLINK} ${CLINKFLAGS} ${FLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

.c.o:
	${CC} ${INSTRU} ${INSTRU_OPTS} ${FLAGS} -c $<

is.o: is.c npbparams.h

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f IS core

veryclean: clean
	- rm -rf .cere/ regions.csv
	- rm -f realmain.c cere.* *.html
