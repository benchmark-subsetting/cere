#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 CERE_MODE="dump --region=__cere__is_main_724 --invocation=1"
    ./IS

    make clean
    make -j4 CERE_REPLAY_REPETITIONS=1 CERE_MODE="replay --region=__cere__is_main_724 --static"
    ./IS > $TMPDIR/test.replay.out

    cat $TMPDIR/test.replay.out | head -10 > $TMPDIR/test.a

    diff $TMPDIR/test.a verif
}

source ../source.sh
