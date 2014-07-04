#!/bin/bash

make veryclean
test_1= ../../src/granularity/coverage.sh . ./IS "make INVITRO_CALL_COUNT=1 -j4"
test_2= ../../src/granularity/matching.sh . ./measures/loops ./IS "make INVITRO_CALL_COUNT=1 -j4"
test_3= ../../src/Report/Report.py ./
$test_1 || $test_2 || $test_3
exit $?
