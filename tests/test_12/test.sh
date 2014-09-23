#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 MODE="dump --region=__extracted__tml_fluxk__1268"
    LD_BIND_NOW=1 ./leslie3d < leslie3d.in

    make clean
    make -j4 INVITRO_CALL_COUNT=1 MODE="replay --region=__extracted__tml_fluxk__1268"
    ./leslie3d < leslie3d.in
}

source ../source.sh
