#!/bin/bash
make veryclean > /dev/null 2>&1
make MODE=--dump  > /dev/null 2>&1
LD_BIND_NOW=1 ./FT > /dev/null 2>&1

make clean > /dev/null 2>&1
make MODE=--replay=__extracted__auxfnct_ilog2__48 > /dev/null 2>&1
exit $?

