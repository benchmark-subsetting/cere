#!/bin/bash

function do_test()
{
    make clean && rm *.ll
    rm -rf cere_measures/ cere_dumps/
    ../../cere configure --build-cmd="make -j2" --run-cmd="./gromacs -silent -deffnm gromacs -nice 0"
    ../../cere capture --region=__cere__fnbf_do_fnbf_232 --invocation=7948 -f
}

source ../source.sh
