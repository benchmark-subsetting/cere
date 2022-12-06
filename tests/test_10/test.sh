#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 CERE_MODE="dump --region=__cere__test_check_11"
    ./test > $TMPDIR/test.dump.out 2>&1

    make clean
    make -j4 CERE_MODE="replay --region=__cere__test_check_11"
    ./test > $TMPDIR/test.replay.out 2>&1

    cat $TMPDIR/test.dump.out | grep "value =" | head -n1 > $TMPDIR/test.a
    cat $TMPDIR/test.replay.out | grep "value =" | head -n1 > $TMPDIR/test.b

    if [ ! -s $TMPDIR/test.a ]; then
        return 1
    fi

    cat $TMPDIR/test.a
    cat $TMPDIR/test.b

    diff $TMPDIR/test.a $TMPDIR/test.b
}

source ../source.sh
