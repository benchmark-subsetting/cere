#!/bin/bash

BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CLASS=A
export WARMUP_TYPE=1
export INVITRO_CALL_COUNT=5

for bench in LU CG EP FT IS MG SP BT; do
    echo "Benchmarking $bench"
    sbench=$( echo $bench | tr '[A-Z]' '[a-z]' )
    cd $BASEDIR/$bench
    RUN_COMMAND="numactl -C 1 ../bin/${sbench}.${CLASS}"
    BUILD_COMMAND="make CLASS=${CLASS}"
    ../../../cere configure --build_cmd="$BUILD_COMMAND" --run_cmd="$RUN_COMMAND"
    ../../../cere profile
    ../../../cere select-max-cov
    ../../../cere report
done
