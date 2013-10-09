SHELL=/bin/sh
BENCHMARK=bt
BENCHMARKU=BT
TOP=../../..

include ../config/make.def

OBJS = bt.s initialize.s exact_solution.s exact_rhs.s \
       set_constants.s adi.s  rhs.s \
       x_solve.s y_solve.s \
       z_solve.s add.s error.s verify.s \
       ${COMMON}/print_results.o ${COMMON}/timers.o ${COMMON}/wtime.o \
       my_lib.o

include ../sys/make.common

# npbparams.h is included by header.h
# The following rule should do the trick but many make programs (not gmake)
# will do the wrong thing and rebuild the world every time (because the
# mod time on header.h is not changed. One solution would be to 
# touch header.h but this might cause confusion if someone has
# accidentally deleted it. Instead, make the dependency on npbparams.h
# explicit in all the lines below (even though dependence is indirect). 

# header.h: npbparams.h

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -lgfortran -o ${PROGRAM} ${OBJS} ${F_LIB}

%.o: %.f
	${FCOMPILE} $<

%.s: %.ll
	llc -march=x86-64 $< -o $@

%.ll: %.f npbparams.h header.h
	$(F77) -S -fplugin=$(TOP)/../libs/dragonegg.so -fplugin-arg-dragonegg-emit-ir $< -o $@
	opt -S -loop-extract $@ -o $@
	opt -S -load $(TOP)/../src/llvm-3.3.src/Release+Asserts/lib/LLVMLoop-instrumentation.so -loop-instrumentation $@ -o $@

my_lib.o:
	make -C $(TOP)/external_lib_example/
	cp $(TOP)/external_lib_example/my_lib.o .

clean:
	- rm -f *.o *.s *.ll *~ mputil*
	- rm -f  npbparams.h core
