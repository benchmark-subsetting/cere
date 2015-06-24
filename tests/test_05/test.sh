#!/bin/bash

function do_test()
{
    make veryclean
    ../../cere configure --build-cmd="make" --run-cmd="./IS"
    ../../cere profile
    ../../cere select-ilp
    ../../cere report
    exit $?
}

source ../source.sh
