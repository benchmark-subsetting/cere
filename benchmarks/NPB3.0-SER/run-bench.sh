#!/bin/bash

BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CLASS=W

for bench in FT; do
    echo "Benchmarking $bench"
    sbench=$( echo $bench | tr '[A-Z]' '[a-z]' )
    cd $BASEDIR/$bench
    RUN_COMMAND="numactl -C 1 ../bin/${sbench}.${CLASS}"
    BUILD_COMMAND="make -j4 CLASS=${CLASS}"
    ../../../src/granularity/coverage.sh "$RUN_COMMAND" "$BUILD_COMMAND" 
    ../../../src/granularity/matching.sh . "measures/loops" "$RUN_COMMAND" "$BUILD_COMMAND" 
    ../../../src/Report/Report.py ./
done
