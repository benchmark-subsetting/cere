#!/bin/bash
make clean
cere configure --run-cmd="./FT" --build-cmd="make CLASS=A" --clean-cmd="make clean"
cere regions

cat regions.csv
exit $?
