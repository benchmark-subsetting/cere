SHELL=/bin/sh
PROJECT_ROOT=../../
CC=../../src/cerec
FLINK=../../src/cerec

FLAGS=-O0

OBJS = test.o

PROGRAM=test

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} 

%.o: %.c
	${CC} ${MODE} -c ${FLAGS} $<

clean:
	- rm -f *.o *.s *.ll *.bin *.csv
	- rm -f test

veryclean: clean
	- rm -rf .cere/
	- rm -f *.o realmain.c regions.csv
