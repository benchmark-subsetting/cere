#!/bin/bash

function do_test()
{
    make veryclean
    cere configure --build-cmd="make -j2" --clean-cmd="make clean" --run-cmd="./BT"
    cere replay --region="__cere__rhs_compute_rhs__59" -f
}

source ../source.sh
