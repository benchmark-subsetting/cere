#!/bin/bash

function do_test()
{
    make veryclean
    cere configure --build-cmd="make" --run-cmd=./test --clean-cmd="make clean" --omp
    export OMP_NUM_THREADS=4
    cere capture --region=__cere__test_fct1_25 --invocation=2
    export OMP_NUM_THREADS=4
    cere replay --region=__cere__test_fct1_25 --invocation=2
    exit 0
}

source ../source.sh
