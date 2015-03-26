#!/bin/bash

function do_test()
{
    rm -rf cere_dumps/ *.ll
    make clean
    make -j4 MODE="dump --region=__cere__fnbf_do_fnbf_232 --invocation=7948"
    LD_BIND_NOW=1 ./gromacs -silent -deffnm gromacs -nice 0

    make clean
    make -j4 INVITRO_CALL_COUNT=1 MODE="replay --region=__cere__fnbf_do_fnbf_232 --invocation=7948 --instrument --lib=../../src/rdtsc/librdtsc.a --wrapper=../../src/ccc/lel/librdtsc_wrapper.a"
    ./gromacs -silent -deffnm gromacs -nice 0 > $TMPDIR/test.replay.out 2>&1

    #diff -u $TMPDIR/test.replay.out verif
}

source ../source.sh
