SHELL=/bin/sh
PROJECT_ROOT=../../
CC=../../src/lec/lec
FLINK=../../src/lel/lel

FLAGS=-O3

OBJS = test.o

PROGRAM=test

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} 

%.o: %.c
	${CC} ${MODE} ${FLAGS} $<

clean:
	- rm -f *.o *.s *.ll *.bin *.csv
	- rm -f test

veryclean: clean
	- rm -rf dump/
	- rm -f *.o lel_bin realmain.c dump.c 
