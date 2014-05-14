#!/bin/bash

TMPDIR=`mktemp -d`
make veryclean
make MODE=--dump
LD_BIND_NOW=1 ./BT

make clean
make MODE=--replay=__extracted__verify_verify__265
./BT > $TMPDIR/test.replay.out

cat $TMPDIR/test.replay.out | head -n1 > $TMPDIR/test.a

diff $TMPDIR/test.a verif

rm -rf "$TMPDIR"
exit $?
