#!/bin/bash

TMPDIR=`mktemp -d`
make veryclean > /dev/null 2>&1
make MODE=--dump  > /dev/null 2>&1
LD_BIND_NOW=1 ./BT > /dev/null 2>&1

make clean > /dev/null 2>&1
make MODE=--replay=__extracted__verify_verify__265 > /dev/null 2>&1
./BT > $TMPDIR/test.replay.out

cat $TMPDIR/test.replay.out | head -n1 > $TMPDIR/test.a

diff $TMPDIR/test.a verif

rm -rf "$TMPDIR"
exit $?
