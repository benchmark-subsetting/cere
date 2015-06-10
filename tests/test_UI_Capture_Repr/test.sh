#!/bin/bash

function do_test()
{
    make veryclean
    ../../cere configure --build-cmd="make -j2" --run-cmd="./BT"
    ../../cere capture --region="__cere__rhs_compute_rhs__59"
}

source ../source.sh
