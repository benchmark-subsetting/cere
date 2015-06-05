#!/bin/bash

function do_test()
{
    rm -rf .cere/
    ../../cere configure --build_cmd="make -j2" --run_cmd="./bwaves"
    ../../cere capture --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6
    ../../cere replay --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6
}

source ../source.sh
