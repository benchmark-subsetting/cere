#!/bin/bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$ROOT/../../"

# Check number of arguments
if [ $# -ne 2 ] ; then
    echo "usage: $0 BENCH_DIR BINARY_NAME"
    exit 1
fi

BENCH_DIR=$1
shift
BIN=$1
shift
COMPILE_CMD=$*

#1) Measure application cycles
cd $BENCH_DIR 2> /dev/null
if [ "$?" != "0" ] ; then
    echo "Could not change directory to $BENCH_DIR" > /dev/stderr
    exit 1
fi

exit 1
make clean
CMD=${COMPILE_CMD}" "INSTRU=--instrument" "INSTRU_OPTS="--instrument-loop=bench"
${CMD}
./${BIN}
if [[ ! -f rdtsc_result.csv ]]; then
    error="Measuring application failed!\n"
else
    mv rdtsc_result.csv app_cycles.csv
    CYCLES=`cat app_cycles.csv | tail -n 1 | cut -d ',' -f 3`
fi
cd ..

#2) We need to instrument all in-vivo loops
cd $BENCH_DIR
make clean
CMD=${COMPILE_CMD}" "INSTRU=--instrument
${CMD}
./${BIN}
if [[ ! -f rdtsc_result.csv ]]; then
    error="$error Measuring in-vivo loops failed!\n"
else
    mv rdtsc_result.csv all_loops.csv
fi
cd ..

if [[ -f $BENCH_DIR/all_loops.csv ]]
then
    cd $BENCH_DIR
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
            warning="Measuring in-vivo loops failed for ${level}!\n"
        else
            ${ROOT}/fusion.py all_loops.csv rdtsc_result.csv
            mv rdtsc_result.csv ${level}.csv
        fi
    done
    cd ..
fi

#5) dump loops
cd $BENCH_DIR
make clean
make MODE=--dump CLASS=B
./${BIN}
cd ..

#6) Measure in-vitro loops
cd $BENCH_DIR
rm -f ${BIN} lel_bin realmain.c
make MODE=--dump CLASS=B #To create lel_bin

if [[ !( -d results ) ]]
then
    mkdir results/
fi
echo "Codelet Name, Call Count, CPU_CLK_UNHALTED_CORE" > results/invitro_results.csv
for loops in `ls dump`
do
    make clean
    rm -f ${bench,,}.B
    rm -f realmain.c
    make MODE=--replay=$loops INSTRU=--instrument CLASS=B
    ./${BIN}
    if [[ -f rdtsc_result.csv ]]; then
        mv rdtsc_result.csv results/${loops}.csv
        cat results/${loops}.csv | tail -n 1 >> results/invitro_results.csv
    else
        warning="$warning Measuring in-vitro cycles for $loops failed$\n"
    fi
done
lines=`wc -l results/invitro_results.csv | cut -d' ' -f1`
if [[ ! "$lines" > 1 ]]
then
    error="$error No invitro measure!\n"
fi
make clean
rm -f ${bench,,}.B
cd ..

if [[ ! -z $warning ]]
then
    echo "Warning, you may have wrong results because:"
    echo -e $warning
fi
if [[ ! -z $error ]]
then
    echo "Error, can't compute coverage and/or matching because:"
    echo -e $error
else
    #7) Now find codelets to keep
    CYCLES=`cat $BENCH_DIR/app_cycles.csv | tail -n 1 | cut -d ',' -f 3`
    ${ROOT}/granularity.py $BENCH_DIR/all_loops.csv ${CYCLES} > $BENCH_DIR/loop_to_keep

    #8) Plot coverage and matching
    ${ROOT}/compute_coverage_matching.R $BENCH_DIR
fi

