#!/bin/bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$ROOT/../../"
RES_DIR="measures"
#~ PLOT_DIR="$RES_DIR/plots"
export LD_BIND_NOW=1

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

#1) Create measures and plots folder
if [[ !( -d $RES_DIR ) ]]
then
    mkdir $RES_DIR
fi
#~ if [[ !( -d $PLOT_DIR ) ]]
#~ then
    #~ mkdir $PLOT_DIR
#~ fi

#2) Measure application cycles
make clean && rm -f *.ll
${COMPILE_CMD} MODE="original" INSTRU_OPTS="--instrument --instrument-app"
eval ${BIN_CMD}
if [[ ! -f rdtsc_result.csv ]]; then
    error="\tMeasuring application failed!\n"
else
    mv rdtsc_result.csv $RES_DIR/app_cycles.csv
fi

#3) We need to instrument all in-vivo loops
make clean && rm -f *.ll
${COMPILE_CMD} MODE="original" INSTRU_OPTS="--instrument"
eval ${BIN_CMD}
if [[ ! -f rdtsc_result.csv ]]; then
    error="$error \tMeasuring in-vivo loops failed!\n"
else
    mv rdtsc_result.csv $RES_DIR/all_loops.csv
fi

if [[ -f $RES_DIR/all_loops.csv ]]
then
    #Create level files
    ${ROOT}/find_loop_levels.py $RES_DIR/all_loops.csv
    mv level_* $RES_DIR/.

    #For each level, measure in-vivo loops
    for level in `ls $RES_DIR/level_*`
    do
        make clean && rm -f *.ll
        ${COMPILE_CMD} MODE="original" INSTRU_OPTS="--instrument --regions-file=${level}"
        eval ${BIN_CMD}
        if [[ ! -f rdtsc_result.csv ]]; then
            warning="\tMeasuring in-vivo loops failed for ${level}!\n"
        else
            ${ROOT}/fusion.py $RES_DIR/all_loops.csv rdtsc_result.csv
            mv rdtsc_result.csv ${level}.csv
        fi
    done
fi

#4) dump loops
#First get all important loops
CYCLES=`cat ${RES_DIR}/app_cycles.csv | tail -n 1 | cut -d ',' -f 3`
${ROOT}/granularity.py ${BENCH_DIR}/${RES_DIR}/all_loops.csv ${CYCLES} -o ${RES_DIR}/loops_to_dump
sed -i 's/__invivo__/__extracted__/g' $RES_DIR/loops_to_dump

make clean && rm -f *.ll
${COMPILE_CMD} MODE="dump --regions-file=$RES_DIR/loops_to_dump"
eval ${BIN_CMD}

#Create a file with all dumped loops name
touch dump/extracted_loops
for files in `ls dump`
do
    if [[ ( -d "dump/$files" ) && ! ( `grep -F ${files} dump/extracted_loops` ) ]]; then
        echo "${files}" >> dump/extracted_loops
    fi
done

#5) Measure in-vitro loops
echo "Codelet Name,Call Count,CPU_CLK_UNHALTED_CORE" > $RES_DIR//invitro_results.csv
while read loops
do
    make clean && rm -f *.ll
    rm -f realmain.c
    ${COMPILE_CMD} MODE="replay --region=$loops" INSTRU="--instrument"
    eval ${BIN_CMD}
    if [[ -f rdtsc_result.csv ]]; then
        mv rdtsc_result.csv $RES_DIR//${loops}.csv
        rdtsclines=`wc -l $RES_DIR/${loops}.csv | cut -d' ' -f1`
        if [[ "$rdtsclines" > 1 ]]
        then
            cat $RES_DIR/${loops}.csv | tail -n 1 >> $RES_DIR/invitro_results.csv
            echo "${loops/__extracted__/__invivo__}" >> $RES_DIR/replayedCodelet
        else
            warning="$warning \tMeasuring in-vitro cycles for $loops failed\n"
        fi
    else
        warning="$warning \tMeasuring in-vitro cycles for $loops failed\n"
    fi
done < dump/extracted_loops

make clean && rm -f *.ll
lines=`wc -l $RES_DIR/invitro_results.csv | cut -d' ' -f1`
if [[ ! "$lines" > 1 ]]
then
    error="$error \tNo invitro measure!\n"
fi

if [[ ! -z $warning ]]
then
    echo "Warning, you may have wrong results because:"
    echo -e $warning
    echo -e $warning >  $RES_DIR/warning
fi
if [[ ! -z $error ]]
then
    echo "Error, can't compute coverage and/or matching because:"
    echo -e $error
    echo -e $error >  $RES_DIR/error
    exit 1
else
    #7) Find matching and replayed codelets
    ${ROOT}/compute_matching.R $BENCH_DIR/$RES_DIR

    #8) Now find codelets to keep
    ${ROOT}/granularity.py $BENCH_DIR/$RES_DIR/all_loops.csv --matching=$BENCH_DIR/$RES_DIR/replayedCodelet ${CYCLES} -o $RES_DIR/loops
    exit 0
fi
