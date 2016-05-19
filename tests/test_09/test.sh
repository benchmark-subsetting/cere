#!/bin/bash

function do_test()
{
    rm -rf .cere/ *.ll
    make clean
    make -j4 CERE_MODE="dump --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6"
    ./bwaves

    make clean
    make -j4 CERE_REPLAY_REPETITIONS=1 CERE_MODE="replay --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6 --instrument --wrapper=-lcere_rdtsc"
    ./bwaves > $TMPDIR/test.replay.out 2>&1
}

source ../source.sh
