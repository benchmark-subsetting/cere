#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 CERE_MODE="dump --region=__cere__test_checksum_7"
    ./test 2> $TMPDIR/test.dump.out
    cat $TMPDIR/test.dump.out

    make clean
    make -j4 CERE_MODE="replay --region=__cere__test_checksum_7"
    ./test 2> $TMPDIR/test.replay.out
    cat $TMPDIR/test.replay.out

    cat $TMPDIR/test.dump.out | grep "&a" | head -n1 > test.a
    cat $TMPDIR/test.replay.out | grep "&a" | head -n1 > test.b
    if [ ! -s test.a ]; then
        return 1
    fi

    diff test.a test.b
}

source ../source.sh
