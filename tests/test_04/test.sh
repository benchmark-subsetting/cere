#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 CERE_MODE="dump --region=__cere__verify_verify__265"
    ./BT

    make clean
    make -j4 CERE_REPLAY_REPETITIONS=2 CERE_MODE="replay --region=__cere__verify_verify__265 --instrument --wrapper=-lcere_rdtsc"
    ./BT

    rm -rf "$TMPDIR"
    if grep "__cere__verify_verify__265,2" __cere__verify_verify__265.csv ; then
        return 0
    else
        return 1
    fi
}

source ../source.sh
