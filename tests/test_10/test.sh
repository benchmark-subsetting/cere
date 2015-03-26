#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 MODE="dump"
    LD_BIND_NOW=1 ./test> $TMPDIR/test.dump.out
    cat $TMPDIR/test.dump.out

    make clean
    make -j4 MODE="replay --region=__cere__test_check_10" F_LIB=-lc
    ./test > $TMPDIR/test.replay.out
    cat $TMPDIR/test.replay.out

    cat $TMPDIR/test.dump.out | grep "value =" | head -n1 > $TMPDIR/test.a
    cat $TMPDIR/test.replay.out | grep "value =" | head -n1 > $TMPDIR/test.b
    
    if [ ! -s $TMPDIR/test.a ]; then
        return 1
    fi
    
    diff $TMPDIR/test.a $TMPDIR/test.b
}

source ../source.sh
