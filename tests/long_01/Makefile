PROJECT_ROOT=../../
FLAGS+=-g
CC=../../src/lec/lec
CLINK=../../src/lel/lel

OBJS = blocksort.o  bzip2.o  bzlib.o  compress.o  crctable.o  decompress.o  huffman.o  randtable.o  spec.o
PROGRAM=bzip2

${PROGRAM}: ${OBJS}
	${CLINK} ${MODE}  ${INSTRU} ${FLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

%.o: %.c
	${CC} ${MODE} ${INSTRU} ${FLAGS} $<

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f $(PROGRAM)
	- rm -f dump.c

veryclean: clean
	- rm -rf dump/ results/
	- rm -f lel_bin realmain.c dump.c level_* loop_to_keep matching_codelets out *.csv dryer.jpg.bz2
