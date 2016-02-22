#!/bin/bash
make veryclean
make -j4 MODE="dump --region=__cere__auxfnct_ilog2__48"
LD_BIND_NOW=1 ./FT

make clean
make MODE="replay --region=__cere__auxfnct_ilog2__48"
./FT | grep '128         128'
exit $?
