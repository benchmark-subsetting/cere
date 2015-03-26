#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 MODE="dump"
    LD_BIND_NOW=1 ./BT

    make clean
    make -j4 INVITRO_CALL_COUNT=2 MODE="replay --region=__cere__verify_verify__265 --instrument --lib=../../src/rdtsc/librdtsc.a --wrapper=../../src/ccc/lel/librdtsc_wrapper.a"
    ./BT

    rm -rf "$TMPDIR"
    if grep "__cere__verify_verify__265,2" __cere__verify_verify__265.csv ; then
        return 0
    else
        return 1
    fi
}

source ../source.sh
