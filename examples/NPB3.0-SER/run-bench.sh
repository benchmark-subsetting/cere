#!/bin/bash

BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CLASS=A
export CERE_WARMUP="WORKLOAD"
export CERE_REPLAY_REPETITIONS=5

for bench in LU CG EP FT IS MG SP BT; do
    echo "Benchmarking $bench"
    sbench=$( echo $bench | tr '[A-Z]' '[a-z]' )
    cd $BASEDIR/$bench
    RUN_COMMAND="numactl -C 1 ../bin/${sbench}.${CLASS}"
    BUILD_COMMAND="make CLASS=${CLASS}"
    cere configure --build-cmd="$BUILD_COMMAND" --run-cmd="$RUN_COMMAND" --clean-cmd="make clean"
    cere profile
    cere select-max-cov
    cere report
done
