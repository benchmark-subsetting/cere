SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT
PROJECT_ROOT=../../
include make.def
CC=../../src/cerec
F77=../../src/cerec
FLINK=../../src/cerec

FLAGS=-g

OBJS = bt.o initialize.o exact_solution.o exact_rhs.o \
       set_constants.o adi.o  rhs.o \
       x_solve.o y_solve.o \
       z_solve.o add.o error.o verify.o \
       print_results.o timers.o wtime.o

PROGRAM=BT

${PROGRAM}: ${OBJS}
	${FLINK} ${FLINKFLAGS} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} -lgfortran

%.o: %.c
	${CC} ${FLAGS} -c $<

%.o: %.f
	${F77} ${FLAGS} -c $<

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f core BT
	- rm -f dump.c

veryclean: clean
	- rm -rf .cere/
	- rm -f lel_bin realmain.c dump.c
