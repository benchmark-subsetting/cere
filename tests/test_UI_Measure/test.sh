#!/bin/bash

function do_test()
{
    cd ../test_09
    rm -rf .cere
    cere configure --build-cmd="make -j2" --clean-cmd="make clean" --run-cmd="./bwaves"
    cere instrument --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6
}

source ../source.sh
