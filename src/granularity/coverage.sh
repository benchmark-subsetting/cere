#!/bin/bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$ROOT/../../"

BIN=$1
shift
COMPILE_CMD=$*

#1) Measure application cycles
CMD=${COMPILE_CMD}" "INSTRU=--instrument" "INSTRU_OPTS="--instrument-loop=bench"
${CMD}
./${BIN}
if [[ ! -f rdtsc_result.csv ]]; then
        echo "Measuring application failed, no result files"
        exit
fi
CYCLES=`cat rdtsc_result.csv | tail -n 1 | cut -d ',' -f 3`
rm rdtsc_result.csv

#2) We need to instrument all in-vivo loops
make clean
CMD=${COMPILE_CMD}" "INSTRU=--instrument
${CMD}
./${BIN}
if [[ ! -f rdtsc_result.csv ]]; then
    echo "Measuring in-vivo loops failed, no result files"
    exit
fi
mv rdtsc_result.csv all_loops.csv

#3) Create level files
${ROOT}/find_loop_levels.py all_loops.csv

#4) For each level, measure in-vivo loops
for level in `ls level_*`
do
    make clean
    CMD=${COMPILE_CMD}" "INSTRU=--instrument" "INSTRU_OPTS="-loops-file=${level}"
    ${CMD}
    ./${BIN}
    if [[ ! -f rdtsc_result.csv ]]; then
        echo "Measuring in-vivo loops failed for ${level}, no result files"
        exit
    fi
    ${ROOT}/fusion.py all_loops.csv rdtsc_result.csv
    rm rdtsc_result.csv ${level}
done

#5) Now find codelets to keep
${ROOT}/granularity.py all_loops.csv ${CYCLES} --min_coverage 1 --min_cycles 10000


