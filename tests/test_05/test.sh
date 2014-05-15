#!/bin/bash

make veryclean
../../src/granularity/coverage.sh . ./IS "make INVITRO_CALL_COUNT=1 -j4"
exit $?
