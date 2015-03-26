#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 MODE="dump --region=__cere__is_main_724 --invocation=1"
    LD_BIND_NOW=1 ./IS

    make clean
    make -j4 INVITRO_CALL_COUNT=1 MODE="replay --region=__cere__is_main_724"
    ./IS > $TMPDIR/test.replay.out

    cat $TMPDIR/test.replay.out | head -10 > $TMPDIR/test.a

    diff $TMPDIR/test.a verif
}

source ../source.sh
