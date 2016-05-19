#!/bin/bash

function do_test()
{
    make veryclean
    cere configure --build-cmd="make" --clean-cmd="make clean" --run-cmd="./IS"
    cere profile --regions
    exit $?
}

source ../source.sh
