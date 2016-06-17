SHELL=/bin/sh
PROJECT_ROOT=../../
CC=../../src/cerec
FLINK=../../src/cerec

FLAGS=-O3 -g

OBJS = test.o

PROGRAM=test

${PROGRAM}: ${OBJS}
	${FLINK} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} 

%.o: %.c
	${CC} ${FLAGS} -c $<

clean:
	- rm -f *.o *.s *.ll
	- rm -f test

veryclean: clean
	- rm -rf .cere/
	- rm -f *.o realmain.c
