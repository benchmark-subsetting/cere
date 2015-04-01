#!/bin/bash

function do_test()
{
    cd ../test_09
    rm -rf cere_measures cere_dumps
    python ../../cere configure --build_cmd="make -j2" --run_cmd="./bwaves"
    python ../../cere dump --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6 -f
    python ../../cere replay --region=__cere__block_solver_bi_cgstab_block__52 --invocation=6
}

source ../source.sh
