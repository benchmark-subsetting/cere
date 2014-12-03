SHELL=/bin/sh
PROJECT_ROOT=../../
CC=../../src/lec/lec
FLINK=../../src/lel/lel

FLAGS=-g -O0

OBJS = test.o

PROGRAM=test

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${FLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB} 

%.o: %.c
	${CC} ${MODE} ${FLAGS} -c $<

clean:
	- rm -f *.o *.s *.ll
	- rm -f test

veryclean: clean
	- rm -rf cere_dumps/
	- rm -f *.o lel_bin realmain.c dump.c
