#!/bin/bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$ROOT/../../"

# Check number of arguments
if [ $# -lt 3 ] || [ $# -gt 5 ]; then
    echo "usage: $0 [--force] [BENCH_DIR] codelets-to-dump-file exec_cmd compile_cmd"
    echo "codelets-to-dump-file must contains loops you want to compute coverage for."
    echo "Those loops can be obtained by running granularity script on replayed codelets"
    exit 1
fi

#this will delete everything
if [[ $1 == --force ]]; then
    force=1
    shift
fi

if [ $# -eq 4 ] ; then
    BENCH_DIR=$(readlink -f $1) # convert BENCH_DIR to absolute path
    shift
else
    BENCH_DIR=${PWD}
fi

FILE=$1
BIN_CMD=$2
COMPIL_CMD=$3

whitespace="[[:space:]]"
if [[ $BIN_CMD =~ $whitespace ]]
then
    BIN_CMD=\"$BIN_CMD\"
fi

rm -r invocation_error matching_error matching_log new_matching_codelets Rplots.pdf

cd $BENCH_DIR 2> /dev/null
if [ "$?" != "0" ] ; then
    echo "Could not change directory to $BENCH_DIR" > /dev/stderr
    exit 1
fi

#Check if everything we need is present
if [[ ! -f all_loops.csv ]] || [[ ! -f app_cycles.csv ]]; then
    echo "all_loops.csv or app_cycles.csv not found."
    echo "Please first run $ROOT/coverage.sh $PWD \"$BIN_CMD\" \"$COMPIL_CMD\""
    exit 1
fi

# compute error |x-y|/max(x,y)
compute_error()
{
    local x=$1
    local y=$2
    diff=`echo "${x}-${y}" | bc`
    if [[ $(echo "if (${x} > ${y}) 1 else 0" | bc) -eq 1 ]]; then
        max=$x
    else
        max=$y
    fi
    diff=${diff/-/} #absolute value :D
    error=`echo "scale=3;${diff}/${max}" | bc | awk '{printf "%f", $0}'`
}

rm -f new_matching_codelets matching_error invocation_error Rplot.pdf
touch new_matching_codelets
#Get application runtime in cycles
app_cycles=`cat app_cycles.csv | tail -n 1 | cut -d ',' -f 3 | tr -d $'\r'`
#For each codelet
while read codeletName; do
    echo "$codeletName"
    make clean &>> out && rm -f *.ll &>> out
    #If invivo trace not already measured, do it
    if [[ ! -f results/$codeletName.csv ]]; then
        echo "Measuring invivo trace"
        ${COMPIL_CMD} MODE="--instrument --instrument-loop=$codeletName --loop-to-trace=$codeletName" &> out
        for i in "1"
        do
            eval ${BIN_CMD} &> out
            mv $codeletName.bin "results/${codeletName}.bin.${i}"
            mv -f rdtsc_result.csv results/$codeletName.csv
        done
    fi
    if [[ ! ( -f  "results/${codeletName}.csv" ) ]]; then
        echo "Error for $codeletName: No measure files for invivo trace"
        continue
    fi
    #We have everything to clusterize performance
    bynaryFiles=`ls results/*$codeletName.bin.*`
    nbLoopFiles=`ls results/*$codeletName.bin.* | wc -l`
    echo "Computing clustering info"
    $ROOT/clusterize_invocations.R $codeletName $nbLoopFiles $bynaryFiles
    if [[ ! ( -f  "${codeletName}.invocations" ) ]]; then
        echo "Error for $codeletName: No clustering infos"
        continue
    fi
    #We have invocations to dump, so let's dump them!
    cycles=0
    result_file="$codeletName"
    while read params; do
        invocation=`echo $params | cut -d ' ' -f 1`
        perc=`echo $params | cut -d ' ' -f 2`
        invivo=`echo $params | cut -d ' ' -f 3`
        if [[ ! -z "$force" ]]; then
            rm -rf dump/${codeletName/__invivo__/__extracted__}/${invocation}
        fi
        #if this invocation is not dumped, do it
        if [[ ! ( -d "dump/${codeletName/__invivo__/__extracted__}/${invocation}" ) ]]; then
            echo "Dumping invocation ${invocation}"
            make clean &>> out && rm -f *.ll &>> out
            ${COMPIL_CMD} MODE="--dump --loop-to-dump=${codeletName/__invivo__/__extracted__} --invocation=${invocation}" &> out
            eval LD_BIND_NOW=1 ${BIN_CMD} &> out
        fi
        if [[ ! ( -d "dump/${codeletName/__invivo__/__extracted__}/${invocation}" ) ]]; then
            echo "No dump for ${codeletName} invocation ${invocation}"
            err=1
            continue
        fi
        #Run the invitro version of this invocation
        if [[ ! -f results/${codeletName/__invivo__/__extracted__}_${invocation}.csv ]]; then
            echo "Running invitro version"
            make clean &>> out && rm -f *.ll &>> out
            ${COMPIL_CMD} MODE="--replay=${codeletName/__invivo__/__extracted__} --invocation=${invocation} --instrument" &> out
            eval ${BIN_CMD} &>> out
            mv -f rdtsc_result.csv results/${codeletName/__invivo__/__extracted__}_${invocation}.csv
        fi
        if [[ ! -f results/${codeletName/__invivo__/__extracted__}_${invocation}.csv ]]; then
            echo "No invitro measure for ${codeletName} invocation ${invocation}"
            err=1
            continue
        fi
        #Compute error between invivo and invitro cycles for this invocation
        tmp_cycles=`cat results/${codeletName/__invivo__/__extracted__}_${invocation}.csv | tail -n 1 | cut -d ',' -f 3`
        tmp_invocation=`cat results/${codeletName/__invivo__/__extracted__}_${invocation}.csv | tail -n 1 | cut -d ',' -f 2`
        c=`echo "${tmp_cycles}/${tmp_invocation}" | bc`
        compute_error ${c} ${invivo}
        echo "In vitro cycles = $c & in vivo cycles = $invivo (error = ${error})"

        #Aggregate invocations cycles, needed to compute invitro measure for the codelet
        cycles=`echo "${cycles}+${c}*${perc}" | bc`
        #And keep track of error for each invocation
        result_file="$result_file ${invocation} ${error}"
    done < ${codeletName}.invocations
    #if an error has occured, let's got to the next codelet
    if [[ ! ( -z ${err} ) ]]; then
        unset err
        rm -f ${codeletName}.invocations
        rm -f results/${codeletName/__invivo__/__extracted__}_*.csv
        continue
    fi
    echo "$result_file" >> invocation_error
    #Compute error between invivo and in vitro
    cc=`grep -F "${codeletName}," all_loops.csv | head -n 1 | cut -d ',' -f 2`
    cy=`grep -F "${codeletName}," all_loops.csv | head -n 1 | cut -d ',' -f 3 | tr -d $'\r'`
    cycles=`echo "scale=3;${cycles}*${cc}" | bc`
    codelet_part=`echo "scale=3;${cy}/${app_cycles}" | bc | awk '{printf "%f", $0}'`

    compute_error ${cycles} ${cy}

    if [[ $error > 0.15 ]]; then
        echo "NOT MATCHING: In vitro = $cycles & invivo = $cy (error = $error, exec = $codelet_part)"
    else
        echo "MATCHING: In vitro = $cycles & invivo = $cy (error = $error, exec = $codelet_part)"
        echo ${codeletName/__invivo__/} >> new_matching_codelets
    fi
    echo "$codeletName $error $codelet_part" >> matching_error
    rm ${codeletName}.invocations
    rm results/${codeletName/__invivo__/__extracted__}_*.csv
done < ${FILE}

#Plot for each codelets the error between invivo and invitro
if [[ ( -f  matching_error ) ]]; then
    $ROOT/density_error.R matching_error
fi
#compute matching and compare old and new method
$PROJECT_ROOT/src/granularity/compute_matching.R .
CYCLES=`cat app_cycles.csv | tail -n 1 | cut -d ',' -f 3`
echo "Before:"
$PROJECT_ROOT/src/granularity/granularity.py all_loops.csv --matching=matching_codelets ${CYCLES}
echo "After:"
$PROJECT_ROOT/src/granularity/granularity.py all_loops.csv --matching=new_matching_codelets ${CYCLES}
