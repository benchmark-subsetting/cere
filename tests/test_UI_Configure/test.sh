#!/bin/bash

function do_test()
{
    cd ../test_07
    ../../cere configure --build_cmd="make -j2" --run_cmd="./gromacs -silent -deffnm gromacs -nice 0"
}

source ../source.sh
