#!/bin/bash

function do_test()
{
    rm -rf cere_dumps/ *.ll
    make clean
    make -j4 MODE="dump --region=__cere__advx1_advx1__622 --invocation=1"
    LD_BIND_NOW=1 ./zeusmp

    make clean
    make -j4 INVITRO_CALL_COUNT=1 MODE="replay --region=__cere__advx1_advx1__622 --invocation=1 --instrument --lib=../../src/rdtsc/librdtsc.a --wrapper=../../src/ccc/lel/librdtsc_wrapper.a"
    ./zeusmp > $TMPDIR/test.replay.out 2>&1
}

source ../source.sh
