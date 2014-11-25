SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT
PROJECT_ROOT=../../
include make.def
CC=../../src/lec/lec
F77=../../src/lec/lec
FLINK=../../src/lel/lel

FLAGS=-g

OBJS = bt.o initialize.o exact_solution.o exact_rhs.o \
       set_constants.o adi.o  rhs.o \
       x_solve.o y_solve.o \
       z_solve.o add.o error.o verify.o \
       print_results.o timers.o wtime.o

PROGRAM=BT

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${FLINKFLAGS} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} -lgfortran

%.o: %.c
	${CC} ${MODE} ${FLAGS} $<

%.o: %.f
	${F77} ${MODE} ${FLAGS} $<

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f core BT
	- rm -f dump.c

veryclean: clean
	- rm -rf cere_dumps/
	- rm -f lel_bin realmain.c dump.c
