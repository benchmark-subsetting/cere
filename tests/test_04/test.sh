#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 CERE_MODE="dump"
    LD_BIND_NOW=1 ./BT

    make clean
    make -j4 INVITRO_CALL_COUNT=2 CERE_MODE="replay --region=__cere__verify_verify__265 --instrument --wrapper=../../src/rdtsc/librdtsc.a"
    ./BT

    rm -rf "$TMPDIR"
    if grep "__cere__verify_verify__265,2" __cere__verify_verify__265.csv ; then
        return 0
    else
        return 1
    fi
}

source ../source.sh
