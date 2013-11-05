#!/bin/bash

make veryclean > /dev/null 2>&1
make MODE=--dump > /dev/null 2>&1
./IS > /dev/null 2>&1

make clean > /dev/null 2>&1
make MODE=--replay=__extracted__is_main_724 > /dev/null 2>&1
./IS > /tmp/test.replay.out

cat /tmp/test.replay.out | head -10 > /tmp/test.a

diff /tmp/test.a verif
exit $?
