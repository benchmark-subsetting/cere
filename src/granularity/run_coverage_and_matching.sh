#!/bin/bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$ROOT/../../"

# Check number of arguments
if [ ! $# -eq 1 ]; then
    echo "usage: $0 BENCH_DIR_FILE"
    exit 1
fi

FILE=$1
ORIGIN_DIR=$PWD

while read benchInfo; do
    benchName=`echo $benchInfo | cut -d ' ' -f 1`
    runCommand=`echo $benchInfo | grep -Po '".*?"'`
    echo "$benchName: $runCommand"
    BENCH_DIR=$(readlink -f $benchName) # convert BENCH_DIR to absolute path
    cd $BENCH_DIR 2> /dev/null
    if [ "$?" != "0" ] ; then
        echo "Could not change directory to $BENCH_DIR"
        continue
    fi
    touch coverage_log matching_log
    $ROOT/coverage.sh "$runCommand" "make INVITRO_CALL_COUNT=1" > coverage_log 2>&1
    $ROOT/matching.sh loops "$runCommand" "make INVITRO_CALL_COUNT=1" > matching_log 2>&1
    cd $ORIGIN_DIR 2> /dev/null
    if [ "$?" != "0" ] ; then
        echo "Could not come back to original directory $ORIGIN_DIR"
        exit 1
    fi
done < ${FILE}
