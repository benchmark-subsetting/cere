SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT
PROJECT_ROOT=../../

include make.def

OBJS = bt.s initialize.s exact_solution.s exact_rhs.s \
       set_constants.s adi.s  rhs.s \
       x_solve.s y_solve.s \
       z_solve.s add.s error.s verify.s \
       print_results.s timers.s wtime.o \
       my_lib.o

PROGRAM=BT

${PROGRAM}: ${OBJS}
	${FLINK} ${FLINKFLAGS} -lgfortran -o ${PROGRAM} ${OBJS} ${F_LIB}

%.o: %.c
	${CC} -c $<

%.o: %.f
	${F77} -c $<

%.s: %.ll
	llc -march=x86-64 $< -o $@

%.ll: %.f npbparams.h header.h
	$(F77) -S -fplugin=$(PROJECT_ROOT)/libs/dragonegg.so -fplugin-arg-dragonegg-emit-ir $< -o $@
	opt -S -load $(PROJECT_ROOT)/src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-extractor-all.so -loop-extract-all $@ -o $@
	opt -S -load $(PROJECT_ROOT)/src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-instrumentation.so -loop-instrumentation $@ -o $@

my_lib.o:
	make -C $(PROJECT_ROOT)/tests/external_lib_example/
	cp $(PROJECT_ROOT)/tests/external_lib_example/my_lib.o .

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f core BT
