#!/bin/bash

function do_test()
{
    make veryclean
    ../../cere configure --build-cmd="make" --run-cmd="./IS"
    ../../cere profile --regions
    exit $?
}

source ../source.sh
