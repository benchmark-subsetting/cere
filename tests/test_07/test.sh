#!/bin/bash

function do_test()
{
    rm -rf ./cere/dumps/ *.ll
    make clean
    make -j4 CERE_MODE="dump --region=__cere__fnbf_do_fnbf_232 --invocation=7948"
    ./gromacs -silent -deffnm gromacs -nice 0

    make clean
    make -j4 CERE_REPLAY_REPETITIONS=1 CERE_MODE="replay --region=__cere__fnbf_do_fnbf_232 --invocation=7948 --instrument --wrapper=-lcere_rdtsc"
    ./gromacs -silent -deffnm gromacs -nice 0 > $TMPDIR/test.replay.out 2>&1

    #diff -u $TMPDIR/test.replay.out verif
}

source ../source.sh
