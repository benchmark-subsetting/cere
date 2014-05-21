#!/bin/bash

TMPDIR=`mktemp -d`
rm -rf dump/ *.ll
make clean
make -j4 MODE="--dump --loop-to-dump=__extracted__advx1_advx1__622 --invocation=1"
LD_BIND_NOW=1 ./zeusmp

make clean
make -j4 INVITRO_CALL_COUNT=1 MODE="--replay=__extracted__advx1_advx1__622 --invocation=1" INSTRU=--instrument
./zeusmp > $TMPDIR/test.replay.out 2>&1
exit $?
