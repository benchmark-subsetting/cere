#!/bin/bash

function do_test()
{
    make veryclean
    make -j4 MODE="dump"
    LD_BIND_NOW=1 ./lbm 20 reference.dat 0 1 100_100_130_ldc.of
}

source ../source.sh
