#!/bin/bash

make veryclean > /dev/null 2>&1
../../src/granularity/coverage.sh . ./IS "make INVITRO_CALL_COUNT=1 -j2" #> /dev/null 2>&1
exit $?
