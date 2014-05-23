#!/bin/bash

TMPDIR=`mktemp -d`
rm -rf dump/ *.ll
make clean
make -j4 MODE="--dump --loop-to-dump=__extracted__block_solver_bi_cgstab_block__52 --invocation=6"
LD_BIND_NOW=1 ./bwaves

make clean
make -j4 INVITRO_CALL_COUNT=1 MODE="--replay=__extracted__block_solver_bi_cgstab_block__52 --invocation=6" INSTRU=--instrument
./bwaves > $TMPDIR/test.replay.out 2>&1
exit $?
