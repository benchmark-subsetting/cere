#!/bin/bash
make veryclean
make -j4 CERE_MODE="dump --region=__cere__auxfnct_ilog2__48"
./FT

make clean
make CERE_MODE="replay --region=__cere__auxfnct_ilog2__48"
./FT | grep '128         128'
exit $?
