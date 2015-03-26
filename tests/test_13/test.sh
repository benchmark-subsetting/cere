#!/bin/bash

function do_test()
{
    export INVITRO_CALL_COUNT=4
    make veryclean
    make MODE="original --region=__cere__test_loop_11 --instrument --trace"
    numactl -C 1 ./test
    ITER2=$(./iter.R __cere__test_loop_11 2)
    ITER4=$(./iter.R __cere__test_loop_11 4)
    echo $ITER2 $ITER4

    make clean
    make MODE="dump --region=__cere__test_loop_11 --invocation=2"
    LD_BIND_NOW=1 numactl -C 1 ./test

    make clean
    make MODE="dump --region=__cere__test_loop_11 --invocation=4"
    LD_BIND_NOW=1 numactl -C 1 ./test

    make clean
    make MODE="replay --region=__cere__test_loop_11 --instrument --trace --invocation=2"
    numactl -C 1 ./test
    REPLAY2=$(./iter.R __cere__test_loop_11 2)

    make clean
    make MODE="replay --region=__cere__test_loop_11 --instrument --trace --invocation=4"
    numactl -C 1 ./test
    REPLAY4=$(./iter.R __cere__test_loop_11 2)

    DIFF2=$(echo "$ITER2 $REPLAY2 - 100 * $ITER2 / d * v p" | dc)
    DIFF4=$(echo "$ITER4 $REPLAY4 - 100 * $ITER4 / d * v p" | dc)
    echo "      diff(ORIGINAL, REPLAY)   = error %"
    echo "it[2] diff($ITER2,   $REPLAY2) = $DIFF2 %"
    echo "it[4] diff($ITER4,   $REPLAY4) = $DIFF4 %"

    if [[ "$HOST" == "tahiti" ]]; then
        if [[ "$DIFF2" -gt "20" ]]; then
            exit 1
        fi

        if [[ "$DIFF4" -gt "20" ]]; then
            exit 1
        fi
    fi

    exit 0
}

source ../source.sh
