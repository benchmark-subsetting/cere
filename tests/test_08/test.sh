#!/bin/bash

function do_test()
{
    rm -rf .cere/ *.ll
    make clean
    make -j4 CERE_MODE="dump --region=__cere__advx1_advx1__622 --invocation=1"
    ./zeusmp

    make clean
    make -j4 CERE_REPLAY_REPETITIONS=1 CERE_MODE="replay --region=__cere__advx1_advx1__622 --invocation=1 --instrument --wrapper=../../src/rdtsc/librdtsc.a"
    ./zeusmp > $TMPDIR/test.replay.out 2>&1
}

source ../source.sh
