#!/bin/bash
make veryclean > /dev/null 2>&1
make MODE=--dump  > /dev/null 2>&1
LD_BIND_NOW=1 ./BT > /dev/null 2>&1

make clean > /dev/null 2>&1
make MODE=--replay=__extracted__verify_verify__265 > /dev/null 2>&1
./BT > /tmp/test.replay.out

cat /tmp/test.replay.out | head -n1 > /tmp/test.a

diff /tmp/test.a verif
exit $?
