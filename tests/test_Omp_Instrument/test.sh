#!/bin/bash

function do_test()
{
    make veryclean
    cere configure --build-cmd="make" --run-cmd=./test --clean-cmd="make clean" --omp
    export OMP_NUM_THREADS=4
    cere instrument --regions-file=reg
    if [ ! -f __cere__test_fct1_25.csv ]
    then
       exit 1
    fi
    if [ ! -f __cere__test_fct1_11.csv ]
    then
       exit 1
    fi
    exit 0
}


source ../source.sh
