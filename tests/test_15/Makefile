SHELL=/bin/sh
PROJECT_ROOT=../../
CC=../../src/ccc/ccc
FLINK=../../src/ccc/ccc

FLAGS=-O3

OBJS = test.o

PROGRAM=test

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} 

%.o: %.c
	${CC} ${MODE} ${FLAGS} -c $<

clean:
	- rm -f *.o *.s *.ll *.bin *.csv
	- rm -f test

veryclean: clean
	- rm -rf .cere/
	- rm -f *.o realmain.c regions.csv
