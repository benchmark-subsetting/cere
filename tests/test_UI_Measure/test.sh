#!/bin/bash

function do_test()
{
    cd ../test_09
    rm -rf cere_measures cere_dumps
    ../../cere configure --build_cmd="make -j2" --run_cmd="./bwaves"
    ../../cere instrument --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6
}

source ../source.sh
