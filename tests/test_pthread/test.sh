#!/bin/bash

function do_test()
{
    make veryclean
    cere configure --build-cmd="make" --run-cmd=./test --clean-cmd="make clean"
    cere capture --region=__cere__test_thread_2_body_7 --invocation=1
    cere replay --region=__cere__test_thread_2_body_7 --invocation=1 --static
    if [ ! -f __cere__test_thread_2_body_7.csv ]
    then
        exit 1
    fi

    exit 0
}

source ../source.sh
