#!/bin/bash
set -e

function do_test()
{
    make veryclean
    cere configure --run-cmd="numactl -C1 ./test" --build-cmd="make" --clean-cmd="make clean"
    cere trace --region=__cere__test_loop_23

    ITER2=$(cere trace --region=__cere__test_loop_23 --read=2)
    ITER4=$(cere trace --region=__cere__test_loop_23 --read=4)
    echo $ITER2 $ITER4

    export CERE_WARMUP="PAGETRACE"
    export CERE_TRACE=1
    export CERE_REPLAY_REPETITIONS=4
    rm -f __cere__test_loop_23.bin
    cere capture --region=__cere__test_loop_23 --invocation=2
    cere replay --region=__cere__test_loop_23 --invocation=2
    cp __cere__test_loop_23.bin .cere/traces/__replay__test_loop_23.bin
    cp __cere__test_loop_23.csv .cere/traces/__replay__test_loop_23.csv
    REPLAY2=$(cere trace --region=__replay__test_loop_23 --read=2)

    rm -f __cere__test_loop_23.bin __replay__test_loop_23.csv
    cere capture --region=__cere__test_loop_23 --invocation=4
    cere replay --region=__cere__test_loop_23 --invocation=4
    cp __cere__test_loop_23.bin .cere/traces/__replay__test_loop_23.bin
    cp __cere__test_loop_23.csv .cere/traces/__replay__test_loop_23.csv
    REPLAY4=$(cere trace --region=__replay__test_loop_23 --read=4)

    DIFF2=$(echo "$ITER2 $REPLAY2 - 100 * $ITER2 / d * v p" | dc)
    DIFF4=$(echo "$ITER4 $REPLAY4 - 100 * $ITER4 / d * v p" | dc)
    echo "      diff(ORIGINAL, REPLAY)   = error %"
    echo "it[2] diff($ITER2,   $REPLAY2) = $DIFF2 %"
    echo "it[4] diff($ITER4,   $REPLAY4) = $DIFF4 %"

    #if [[ "$HOST" == "tahiti" ]]; then
    #    if [[ "$DIFF2" -gt "30" ]]; then
    #       exit 1
    #    fi

    #    if [[ "$DIFF4" -gt "30" ]]; then
    #        exit 1
    #    fi
    #fi

    exit 0
}

source ../source.sh
