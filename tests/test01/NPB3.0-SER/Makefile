SHELL=/bin/sh
CLASS=W
SFILE=config/suite.def

default: header
	@ sys/print_instructions

BT: bt
bt: header
	cd BT; $(MAKE) CLASS=$(CLASS)

IS: is
is: header
	cd IS; $(MAKE) CLASS=$(CLASS)

# Awk script courtesy cmg@cray.com
suite:
	@ awk '{ if ($$1 !~ /^#/ &&  NF > 0)                              \
	printf "make %s CLASS=%s\n", $$1, $$2 }' $(SFILE)  \
	| $(SHELL)

# It would be nice to make clean in each subdirectory (the targets
# are defined) but on a really clean system this will won't work
# because those makefiles need config/make.def
clean:
	- rm -f core 
	- rm -f *~ */core */*~ */*.o */npbparams.h */*.obj */*.exe
	- rm -f sys/setparams sys/makesuite sys/setparams.h

veryclean: clean
	- rm config/make.def config/suite.def Part*
	- rm -f bin/bt.* bin/is.*

header:
	@ sys/print_header

kit: 
	- makekit -s100k -k30 * */* */*/*


