#!/bin/bash

function do_test()
{
    rm -rf cere_dumps/ *.ll
    make clean
    make -j4 MODE="dump --region=__extracted__block_solver_bi_cgstab_block__52 --invocation=6"
    LD_BIND_NOW=1 ./bwaves

    make clean
    make -j4 INVITRO_CALL_COUNT=1 MODE="replay --region=__extracted__block_solver_bi_cgstab_block__52 --invocation=6" INSTRU="--instrument"
    ./bwaves > $TMPDIR/test.replay.out 2>&1
}

source ../source.sh
