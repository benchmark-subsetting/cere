PROJECT_ROOT=../../
CC=../../src/lec/lec
FLINK=../../src/lel/lel

ifdef INSTRU
    F_LIB+=$(PROJECT_ROOT)/src/rdtsc/rdtsc.a
endif

OBJS = blocksort.o  bzip2.o  bzlib.o  compress.o  crctable.o  decompress.o  huffman.o  randtable.o  spec.o dump.o 
PROGRAM=bzip2

${PROGRAM}: ${OBJS}
	${FLINK} ${MODE} ${INSTRU} ${FLINKFLAGS} -lgfortran -o ${PROGRAM} ${OBJS} ${F_LIB}

%.o: %.c
	${CC} ${MODE} ${INSTRU} $<

%.o: %.f
	${F77} ${MODE} ${INSTRU} $<

dump.o:
	cp $(PROJECT_ROOT)/src/memory_dump/dump.c .
	clang -c dump.c

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f $(PROGRAM)
	- rm -f dump.c

veryclean: clean
	- rm -rf dump/
	- rm -f lel_bin realmain.c dump.c
