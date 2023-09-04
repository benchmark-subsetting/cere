SHELL=/bin/sh
PROJECT_ROOT=../../
CC=cerec
FLINK=cerec

FLAGS=-g -O0

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
