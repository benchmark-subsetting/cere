#!/bin/bash

function do_test()
{
    cd ../test_07
    cere configure --build-cmd="make -j2" --clean-cmd="make clean" --run-cmd="./gromacs -silent -deffnm gromacs -nice 0"
}

source ../source.sh
