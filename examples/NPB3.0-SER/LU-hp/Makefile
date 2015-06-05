SHELL=/bin/sh
BENCHMARK=lu
BENCHMARKU=LU

include ../config/make.def

OBJS = lu.o read_input.o \
       domain.o setcoeff.o setbv.o exact.o setiv.o \
       erhs.o ssor.o calcnp.o \
       rhs.o l2norm.o jacld.o blts.o jacu.o buts.o error.o \
       pintgr.o verify.o ${COMMON}/print_results.o \
       ${COMMON}/timers.o ${COMMON}/wtime.o

include ../sys/make.common


# npbparams.h is included by applu.incl
# The following rule should do the trick but many make programs (not gmake)
# will do the wrong thing and rebuild the world every time (because the
# mod time on header.h is not changed. One solution would be to 
# touch header.h but this might cause confusion if someone has
# accidentally deleted it. Instead, make the dependency on npbparams.h
# explicit in all the lines below (even though dependence is indirect). 

# applu.incl: npbparams.h

${PROGRAM}: config ${OBJS}
	${FLINK} ${FLINKFLAGS} -o ${PROGRAM} ${OBJS} ${F_LIB}

.f.o :
	${FCOMPILE} $<

lu.o:		lu.f applu.incl npbparams.h
blts.o:		blts.f
buts.o:		buts.f	
calcnp.o:	calcnp.f applu.incl npbparams.h
erhs.o:		erhs.f applu.incl npbparams.h
error.o:	error.f applu.incl npbparams.h
exact.o:	exact.f applu.incl npbparams.h
jacld.o:	jacld.f applu.incl npbparams.h
jacu.o:		jacu.f applu.incl npbparams.h
l2norm.o:	l2norm.f
pintgr.o:	pintgr.f applu.incl npbparams.h
read_input.o:	read_input.f applu.incl npbparams.h
rhs.o:		rhs.f applu.incl npbparams.h
setbv.o:	setbv.f applu.incl npbparams.h
setiv.o:	setiv.f applu.incl npbparams.h
setcoeff.o:	setcoeff.f applu.incl npbparams.h
ssor.o:		ssor.f applu.incl npbparams.h
domain.o:	domain.f applu.incl npbparams.h
verify.o:	verify.f applu.incl npbparams.h

clean:
	- /bin/rm -f npbparams.h
	- /bin/rm -f *.o *~
