#!/bin/bash

function do_test()
{
    make clean && rm *.ll
    rm -rf cere_measures/ cere_dumps/
    python ../../cere configure --build_cmd="make -j2" --run_cmd="./gromacs -silent -deffnm gromacs -nice 0"
    python ../../cere dump --region=__cere__fnbf_do_fnbf_232 --invocation=7948 -f
}

source ../source.sh
