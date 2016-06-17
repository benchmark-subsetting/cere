#!/bin/bash

function do_test()
{
    make veryclean
    cere configure --build-cmd="make" --run-cmd=./test --clean-cmd="make clean" --omp
    cere instrument --regions-file=reg --omp-num-threads=4
    cat __cere__test_fct1_25.csv __cere__test_fct1_first.csv
    exit 0
}

source ../source.sh
