#!/bin/bash

TMPDIR=`mktemp -d`

make veryclean > /dev/null 2>&1
make MODE=--dump  > /dev/null 2>&1
LD_BIND_NOW=1 ./BT > /dev/null 2>&1

make clean > /dev/null 2>&1
make MODE=--replay=__extracted__verify_verify__265 INSTRU=--instrument > /dev/null 2>&1
./BT > $TMPDIR/test.replay.out

rm -rf "$TMPDIR"
if [ -f rdtsc_result.csv ]; then
    exit 0
else
    exit 1
fi
