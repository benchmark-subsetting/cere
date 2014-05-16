#!/bin/bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$ROOT/../../"

# Check number of arguments
if [ $# -lt 2 ] || [ $# -gt 3 ]; then
    echo "usage: $0 [BENCH_DIR] BINARY_CMD COMPILE_CMD"
    echo "If [BENCH_DIR] not given, this will test the current working directory"
    exit 1
fi

if [ $# -eq 2 ] ; then
    BENCH_DIR=${PWD}
else
    BENCH_DIR=$(readlink -f $1) # convert BENCH_DIR to absolute path
    shift
fi
BIN_CMD=$1
COMPILE_CMD=$2

cd $BENCH_DIR 2> /dev/null
if [ "$?" != "0" ] ; then
    echo "Could not change directory to $BENCH_DIR" > /dev/stderr
    exit 1
fi

#1) Measure application cycles
make clean && rm -f *.ll
${COMPILE_CMD} INSTRU=--instrument INSTRU_OPTS="--instrument-loop=bench"
eval ${BIN_CMD} #> out
if [[ ! -f rdtsc_result.csv ]]; then
    error="\tMeasuring application failed!\n"
else
    mv rdtsc_result.csv app_cycles.csv
fi

#2) We need to instrument all in-vivo loops
make clean && rm -f *.ll
${COMPILE_CMD} INSTRU=--instrument
eval ${BIN_CMD} #> out
if [[ ! -f rdtsc_result.csv ]]; then
    error="$error \tMeasuring in-vivo loops failed!\n"
else
    mv rdtsc_result.csv all_loops.csv
fi

if [[ -f all_loops.csv ]]
then
    #3) Create level files
    ${ROOT}/find_loop_levels.py all_loops.csv

    #4) For each level, measure in-vivo loops
    for level in `ls level_*`
    do
        make clean && rm -f *.ll
        ${COMPILE_CMD} INSTRU=--instrument INSTRU_OPTS="--loops-file=${level}"
        eval ${BIN_CMD} #> out
        if [[ ! -f rdtsc_result.csv ]]; then
            warning="\tMeasuring in-vivo loops failed for ${level}!\n"
        else
            ${ROOT}/fusion.py all_loops.csv rdtsc_result.csv
            mv rdtsc_result.csv ${level}.csv
        fi
    done
fi

#5) dump loops
make clean && rm -f *.ll
${COMPILE_CMD} MODE=--dump
eval LD_BIND_NOW=1 ${BIN_CMD} > out
#Create a file with all dumped loops name
touch dump/extracted_loops
for files in `ls dump`
do
    if [[ ( -d "dump/$files" ) && ! ( `grep -F ${files} dump/extracted_loops` ) ]]; then
        echo "${files}" >> dump/extracted_loops
    fi
done

#6) Measure in-vitro loops
if [[ !( -d results ) ]]
then
    mkdir results/
fi
echo "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE" > results/invitro_results.csv
while read loops
do
    make clean && rm -f *.ll
    rm -f realmain.c
    ${COMPILE_CMD} MODE=--replay=$loops INSTRU=--instrument
    eval ${BIN_CMD} #> out
    if [[ -f rdtsc_result.csv ]]; then
        mv rdtsc_result.csv results/${loops}.csv
        rdtsclines=`wc -l results/${loops}.csv | cut -d' ' -f1`
        if [[ "$rdtsclines" > 1 ]]
        then
            cat results/${loops}.csv | tail -n 1 >> results/invitro_results.csv
        else
            warning="$warning \tMeasuring in-vitro cycles for $loops failed\n"
        fi
    else
        warning="$warning \tMeasuring in-vitro cycles for $loops failed\n"
    fi
done < dump/extracted_loops

make clean && rm -f *.ll
lines=`wc -l results/invitro_results.csv | cut -d' ' -f1`
if [[ ! "$lines" > 1 ]]
then
    error="$error \tNo invitro measure!\n"
fi

if [[ ! -z $warning ]]
then
    echo "Warning, you may have wrong results because:"
    echo -e $warning
    echo -e $warning > warning
fi
if [[ ! -z $error ]]
then
    echo "Error, can't compute coverage and/or matching because:"
    echo -e $error
    echo -e $error > error
    exit 1
else
    #7) Find matching and replayed codelets
    ${ROOT}/compute_matching.R $BENCH_DIR

    #8) Now find codelets to keep
    CYCLES=`cat app_cycles.csv | tail -n 1 | cut -d ',' -f 3`
    ${ROOT}/granularity.py $BENCH_DIR/all_loops.csv --matching=$BENCH_DIR/replayedCodelet ${CYCLES} > loops
    exit 0
fi
