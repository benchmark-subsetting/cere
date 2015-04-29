SHELL=/bin/sh
BENCHMARK=ft
BENCHMARKU=FT
PROJECT_ROOT=../../
include make.def
CC=../../src/ccc/ccc
F77=../../src/ccc/ccc
FLINK=../../src/ccc/ccc
FLAGS=-g -mcmodel=large

OBJS = appft.o auxfnct.o fft3d.o mainft.o verify.o \
       randi8.o print_results.o \
       timers.o wtime.o

PROGRAM=FT

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${INSTRU} ${FLINKFLAGS} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} -lm -lgfortran

%.o: %.c
	${CC} ${MODE} ${INSTRU} ${FLAGS} -c $<

.f.o:
	${F77} ${MODE} ${INSTRU} ${FLAGS} -c $<

appft.o:	appft.f  global.h npbparams.h
auxfnct.o:	auxfnct.f  global.h npbparams.h
fft3d.o:	fft3d.f  global.h npbparams.h
mainft.o:	mainft.f  global.h npbparams.h
verify.o:	verify.f  global.h npbparams.h

clean:
	- rm -f *.o *~ mputil* *.s *.ll
	- rm -f ft core

veryclean: clean
	- rm -rf .cere/
	- rm -f realmain.c regions.csv
