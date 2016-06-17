#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 CERE_MODE="dump --region=__cere__tml_fluxk__1268"
    ./leslie3d < leslie3d.in

    make clean
    make -j4 CERE_REPLAY_REPETITIONS=1 CERE_MODE="replay --region=__cere__tml_fluxk__1268"
    ./leslie3d < leslie3d.in
}

source ../source.sh
