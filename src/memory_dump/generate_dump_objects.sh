#!/bin/sh
for s in $*; do
    n=`basename $s`
    echo ".section s$n, \"aw\"" > $s.S
    echo ".incbin \"$s.memdump\"" >> $s.S
done

for s in $*; do
    n=`basename $s`
	echo -n "-Wl,--section-start=s$n=0x$n "
done
