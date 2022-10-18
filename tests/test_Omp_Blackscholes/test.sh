#!/bin/bash

function do_test()
{
    rm -rf .cere/ blackscholes *.csv
    export OMP_NUM_THREADS=4
    cere configure --build-cmd="make" --run-cmd="./blackscholes $OMP_NUM_THREADS in_64K.txt out" --clean-cmd="make clean" --omp
    cere capture --region=__cere__blackscholes_m4__Z9bs_threadPv_368 --invocation=10
    export OMP_NUM_THREADS=2
    cere replay --region=__cere__blackscholes_m4__Z9bs_threadPv_368 --invocation=10
    if [ ! -f __cere__blackscholes_m4__Z9bs_threadPv_368.csv ]
    then 
        exit 1
    fi
    exit 0
}

source ../source.sh
