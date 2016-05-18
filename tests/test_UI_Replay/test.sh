#!/bin/bash

function do_test()
{
    rm -rf .cere/
    cere configure --build-cmd="make -j2" --clean-cmd="make clean" --run-cmd="./bwaves"
    cere capture --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6
    cere replay --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6
}

source ../source.sh
