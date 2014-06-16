#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 MODE="dump"
    LD_BIND_NOW=1 ./BT

    make clean
    make -j4 INVITRO_CALL_COUNT=2 MODE="replay --region=__extracted__verify_verify__265" INSTRU=--instrument
    ./BT

    rm -rf "$TMPDIR"
    if grep "__extracted__verify_verify__265,1" rdtsc_result.csv ; then
        return 0
    else
        return 1
    fi
}

source ../source.sh
