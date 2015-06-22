#!/bin/bash

function do_test()
{
    export CERE_OMP=1
    make veryclean
    make -j4 MODE="dump --region=__cere__test_fct1_25 --invocation=2" -B

    export OMP_NUM_THREADS=1
    LD_BIND_NOW=1 ./test 2> $TMPDIR/test.dump.out

    export OMP_NUM_THREADS=4
    make clean
    make -j4 MODE="replay --region=__cere__test_fct1_25 --invocation=2 --instrument --wrapper=../../src/rdtsc/librdtsc.a"  -B
    ./test
    make MODE="original  --instrument --regions-file=reg  --wrapper=/home/mpopov/merge_cere_pcere/try4_final/llvm_loop_extractor/src/rdtsc/librdtsc.a --wrapper=../../src/rdtsc/librdtsc.a" -B
    ./test
}

source ../source.sh
