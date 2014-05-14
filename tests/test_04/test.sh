#!/bin/bash

TMPDIR=`mktemp -d`
make veryclean
make MODE=--dump
LD_BIND_NOW=1 ./BT

make clean
make MODE=--replay=__extracted__verify_verify__265 INSTRU=--instrument
./BT

rm -rf "$TMPDIR"
if [ -f rdtsc_result.csv ]; then
    exit 0
else
    exit 1
fi
