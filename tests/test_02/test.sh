#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 CERE_MODE="dump --region=__cere__verify_verify__265"
    ./BT

    make clean
    make -j4 CERE_REPLAY_REPETITIONS=1 CERE_MODE="replay --region=__cere__verify_verify__265"
    ./BT > $TMPDIR/test.replay.out

    cat $TMPDIR/test.replay.out

    cat $TMPDIR/test.replay.out | head -n1 > $TMPDIR/test.a

    diff $TMPDIR/test.a verif
}

source ../source.sh
