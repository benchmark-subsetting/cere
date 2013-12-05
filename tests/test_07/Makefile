PROJECT_ROOT=../../
FLAGS+=-g -O3
CC=../../src/lec/lec
CLINK=../../src/lel/lel
C_LIB+=-lm -lstdc++

ifdef INSTRU
    C_LIB+=$(PROJECT_ROOT)/src/rdtsc/rdtsc.a -lstdc++
endif

OBJS =ComputeList.o  ComputeNonbondedUtil.o  Compute.o  erf.o  LJTable.o  Molecule.o  PatchList.o  Patch.o  ResultSet.o  SimParameters.o  spec_namd.o
PROGRAM=namd

${PROGRAM}: ${OBJS}
	${CLINK} ${MODE}  ${INSTRU} ${FLAGS} -o ${PROGRAM} ${OBJS} ${C_LIB}

%.o: %.c
	${CC} ${MODE} ${INSTRU} ${FLAGS} $<

%.o: %.C
	${CC} ${MODE} ${INSTRU} ${FLAGS} $<


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
