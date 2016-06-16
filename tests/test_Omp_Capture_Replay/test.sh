#!/bin/bash

function do_test()
{
    make veryclean
    cere configure --build-cmd="make" --run-cmd=./test --clean-cmd="make clean" --omp
    cere capture --region=__cere__test_fct1_25 --invocation=2 --omp-num-threads=1
    cere replay --region=__cere__test_fct1_25 --invocation=2 --omp-num-threads=4
    exit 0
}

source ../source.sh
