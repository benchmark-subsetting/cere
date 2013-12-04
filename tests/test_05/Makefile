SHELL=/bin/sh
BENCHMARK=ft
BENCHMARKU=FT
PROJECT_ROOT=../../
include make.def
CC=../../src/lec/lec
F77=../../src/lec/lec
FLINK=../../src/lel/lel
FLAGS=-g -mcmodel=large

ifdef INSTRU
    F_LIB+=$(PROJECT_ROOT)/src/rdtsc/rdtsc.a -lstdc++
endif

OBJS = appft.o auxfnct.o fft3d.o mainft.o verify.o \
       randi8.o print_results.o \
       timers.o wtime.o dump.o

PROGRAM=FT

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${INSTRU} ${FLINKFLAGS} ${FLAGS} -lm -lgfortran -o ${PROGRAM} ${OBJS} ${F_LIB}

dump.o:
	cp $(PROJECT_ROOT)/src/memory_dump/dump.c .
	clang -c dump.c

%.o: %.c
	${CC} ${MODE} ${INSTRU} ${FLAGS} $<

.f.o:
	${F77} ${MODE} ${INSTRU} ${FLAGS} $<

appft.o:	appft.f  global.h npbparams.h
auxfnct.o:	auxfnct.f  global.h npbparams.h
fft3d.o:	fft3d.f  global.h npbparams.h
mainft.o:	mainft.f  global.h npbparams.h
verify.o:	verify.f  global.h npbparams.h

clean:
	- rm -f *.o *~ mputil* *.s *.ll
	- rm -f ft core

veryclean: clean
	- rm -rf dump/
	- rm -f lel_bin realmain.c dump.c
