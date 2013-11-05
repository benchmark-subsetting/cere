SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT
PROJECT_ROOT=../../
include make.def
CC=../../src/lec/lec
F77=../../src/lec/lec
FLINK=../../src/lel/lel



OBJS = bt.o initialize.o exact_solution.o exact_rhs.o \
       set_constants.o adi.o  rhs.o \
       x_solve.o y_solve.o \
       z_solve.o add.o error.o verify.o \
       print_results.o timers.o wtime.o \
       dump.o 

PROGRAM=BT

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${FLINKFLAGS} -lgfortran -o ${PROGRAM} ${OBJS} ${F_LIB}

%.o: %.c
	${CC} ${MODE} $<

%.o: %.f
	${F77} ${MODE} $<

dump.o:
	cp $(PROJECT_ROOT)/src/memory_dump/dump.c .
	clang -c dump.c

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f core BT
	- rm -f dump.c

veryclean: clean
	- rm -rf dump/
	- rm -f lel_bin realmain.c dump.c
