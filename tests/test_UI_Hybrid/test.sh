#!/bin/bash

function do_test()
{
    make clean
    cere configure --build-cmd="make" --run-cmd="./IS" --clean-cmd="make clean"
    cere regions
    cere hybrid --regions-file=.cere/flags/regions_flags.csv --instrument
    ./IS

    cat main.csv
}

source ../source.sh
