#!/bin/bash

function do_test()
{
    make veryclean
    rm -rf .cere/
    cere configure --build-cmd="make" --clean-cmd="make veryclean" --run-cmd="./IS"
    cere capture --region=__cere__is_create_seq_341 --invocation=1 -f
}

source ../source.sh
