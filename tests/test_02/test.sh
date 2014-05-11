#!/bin/bash

TMPDIR=`mktemp -d`
make veryclean > /dev/null 2>&1
make MODE=--dump > /dev/null 2>&1
LD_BIND_NOW=1 ./IS > /dev/null 2>&1

make clean > /dev/null 2>&1
make MODE=--replay=__extracted__is_main_724 > /dev/null 2>&1
./IS > $TMPDIR/test.replay.out

cat $TMPDIR/test.replay.out | head -10 > $TMPDIR/test.a

diff $TMPDIR/test.a verif

rm -rf "$TMPDIR"
exit $?
