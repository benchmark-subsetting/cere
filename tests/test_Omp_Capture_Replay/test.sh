#!/bin/bash

function do_test()
{
    rm -f *.csv
    make veryclean
    cere configure --build-cmd="make" --run-cmd=./test --clean-cmd="make clean" --omp
    export OMP_NUM_THREADS=4
    cere capture --region=__cere__test_fct1_25 --invocation=2
    export OMP_NUM_THREADS=4
    cere replay --region=__cere__test_fct1_25 --invocation=2
    if [ ! -f __cere__test_fct1_25.csv ]
    then
         exit 1
    fi
    exit 0
}

source ../source.sh
