SHELL=/bin/sh
CC=cerec
FLINK=cerec

FLAGS=-g -O0 -fopenmp

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
	- rm -rf .cere/
	- rm -f *.o realmain.c cere.log *.csv lel_bin test.log out cere.json *.ll
