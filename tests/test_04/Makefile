PROJECT_ROOT=../../
FLAGS+=-g
CC=../../src/lec/lec
CLINK=../../src/lel/lel

ifdef INSTRU
    C_LIB+=$(PROJECT_ROOT)/src/rdtsc/rdtsc.a -lstdc++
endif

OBJS = blocksort.o  bzip2.o  bzlib.o  compress.o  crctable.o  decompress.o  huffman.o  randtable.o  spec.o dump.o 
PROGRAM=bzip2

${PROGRAM}: ${OBJS}
	${CLINK} ${MODE}  ${INSTRU} ${FLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

%.o: %.c
	${CC} ${MODE} ${INSTRU} ${FLAGS} $<

dump.o:
	cp $(PROJECT_ROOT)/src/memory_dump/dump.c .
	clang -c dump.c

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f $(PROGRAM)
	- rm -f dump.c

veryclean: clean
	- rm -rf dump/ results/
	- rm -f lel_bin realmain.c dump.c
