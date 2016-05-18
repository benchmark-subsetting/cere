#!/bin/bash

function do_test()
{
    export CERE_OMP=1
    make veryclean
    make -j4 CERE_MODE="dump --region=__cere__test_fct1_25 --invocation=2" -B

    export OMP_NUM_THREADS=1
    ./test 2> $TMPDIR/test.dump.out

    export OMP_NUM_THREADS=4
    make clean
    make -j4 CERE_MODE="replay --region=__cere__test_fct1_25 --invocation=2 --instrument --wrapper=../../src/rdtsc/librdtsc.a"  -B
    ./test
    make CERE_MODE="original  --instrument --regions-file=reg --wrapper=../../src/rdtsc/librdtsc.a" -B
    ./test
    unset CERE_OMP
}

source ../source.sh
