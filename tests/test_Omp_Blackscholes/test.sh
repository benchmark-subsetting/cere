#!/bin/bash

function do_test()
{
    rm -rf .cere/ blackscholes
    cere configure --build-cmd="make" --run-cmd="./blackscholes 1 in_64K.txt out" --clean-cmd="make clean" --omp
    export OMP_NUM_THREADS=8
    cere capture --region=__cere__blackscholes_m4__Z9bs_threadPv_368 --invocation=10
    export OMP_NUM_THREADS=4
    cere replay --region=__cere__blackscholes_m4__Z9bs_threadPv_368 --invocation=10
    exit 0
}

source ../source.sh
