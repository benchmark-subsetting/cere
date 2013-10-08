#!/bin/sh
for s in $*; do
    echo ".section s$s, \"aw\"" > $s.S
    echo ".incbin \"$s.memdump\"" >> $s.S
done

for s in $*; do
	echo -n "-Wl,--section-start=s$s=0x$s "
done
