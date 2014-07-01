#!/bin/bash
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$ROOT/../../"
RES_DIR="measures"
PLOT_DIR="$RES_DIR/plots"
export LD_BIND_NOW=1

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

cd $BENCH_DIR 2> /dev/null
if [ "$?" != "0" ] ; then
    echo "Could not change directory to $BENCH_DIR" > /dev/stderr
    exit 1
fi

if [[ !( -d $PLOT_DIR ) ]]
then
    mkdir $PLOT_DIR
fi

#Check if everything we need is present
if [[ ! -f $RES_DIR/all_loops.csv ]] || [[ ! -f $RES_DIR/app_cycles.csv ]]; then
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

rm -f $RES_DIR/new_matching_codelets $RES_DIR/matching_error.csv $RES_DIR/invocation_error $RES_DIR/Rplot.pdf $PLOT_DIR/*
touch $RES_DIR/new_matching_codelets
#Get application runtime in cycles
app_cycles=`cat $RES_DIR/app_cycles.csv | tail -n 1 | cut -d ',' -f 3 | tr -d $'\r'`
echo "Codelet Name,Error,Exec Time" > $RES_DIR/matching_error.csv
#For each codelet
while read codeletName; do
    echo "$codeletName"
    make clean &>> out && rm -f *.ll &>> out
    #If invivo trace not already measured, do it
    if [[ ! -f $RES_DIR/$codeletName.csv ]]; then
        echo "Measuring invivo trace"
        ${COMPIL_CMD} MODE="original --instrument --region=$codeletName --trace" #&>> out
        for i in "1"
        do
            eval ${BIN_CMD} #&>> out
            mv $codeletName.bin "$RES_DIR/${codeletName}.bin.${i}"
            mv -f rdtsc_result.csv $RES_DIR/$codeletName.csv
        done
    fi
    if [[ ! ( -f  "$RES_DIR/${codeletName}.csv" ) ]]; then
        echo "Error for $codeletName: No measure files for invivo trace"
        continue
    fi
    #We have everything to clusterize performance
    bynaryFiles=`ls $RES_DIR/*$codeletName.bin.*`
    nbLoopFiles=`ls $RES_DIR/*$codeletName.bin.* | wc -l`
    echo "Ploting"
    gnuplot -e "filename='$bynaryFiles'" -e "outputFile='${codeletName}'" $ROOT/plot_trace.gnu
    if [[ ! ( -f  "${codeletName}.png" ) ]]; then
        echo "No plots for $codeletName"
    else
        mv ${codeletName}.png $PLOT_DIR/.
    fi
    echo "Computing clustering info"
    $ROOT/clusterize_invocations.R $codeletName $nbLoopFiles $RES_DIR/${codeletName}.csv $bynaryFiles
    if [[ ! ( -f  "${codeletName}.invocations" ) ]]; then
        echo "Error for $codeletName: No clustering infos"
        continue
    fi
    mv ${codeletName}.invocations $RES_DIR/.
    #We have invocations to dump, so let's dump them!
    cycles=0
    result_file="$codeletName"
    while read params; do
        invocation=`echo $params | cut -d ' ' -f 1`
        perc=`echo $params | cut -d ' ' -f 2`
        invivo=`echo $params | cut -d ' ' -f 3`
        if [[ $invivo -eq 0 ]]; then
            echo "Error invivo cycles=$invivo. Can't measure invitro cycles"
            err=1
            continue
        fi
        if [[ ! -z "$force" ]]; then
            rm -rf dump/${codeletName/__invivo__/__extracted__}/${invocation}
        fi
        #if this invocation is not dumped, do it
        if [[ ! ( -d "dump/${codeletName/__invivo__/__extracted__}/${invocation}" ) ]]; then
            echo "Dumping invocation ${invocation}"
            make clean &>> out && rm -f *.ll &>> out
            ${COMPIL_CMD} MODE="dump --region=${codeletName/__invivo__/__extracted__} --invocation=${invocation}" &> out
            eval ${BIN_CMD} &> out
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
            ${COMPIL_CMD} MODE="replay --region=${codeletName/__invivo__/__extracted__} --invocation=${invocation} --instrument" &> out
            eval ${BIN_CMD} &>> out
            mv -f rdtsc_result.csv $RES_DIR/${codeletName/__invivo__/__extracted__}_${invocation}.csv
        fi
        if [[ ! -f $RES_DIR/${codeletName/__invivo__/__extracted__}_${invocation}.csv ]]; then
            echo "No invitro measure for ${codeletName} invocation ${invocation}"
            err=1
            continue
        fi
        #Compute error between invivo and invitro cycles for this invocation
        tmp_cycles=`cat $RES_DIR/${codeletName/__invivo__/__extracted__}_${invocation}.csv | tail -n 1 | cut -d ',' -f 3`
        tmp_invocation=`cat $RES_DIR/${codeletName/__invivo__/__extracted__}_${invocation}.csv | tail -n 1 | cut -d ',' -f 2`
        c=`echo "${tmp_cycles}/${tmp_invocation}" | bc`
        compute_error ${c} ${invivo}
        echo "In vitro cycles = $c & in vivo cycles = $invivo (error = ${error})"

        #Aggregate invocations cycles, needed to compute invitro measure for the codelet
        cycles=`echo "${cycles}+${c}*${perc}" | bc`
        #And keep track of error for each invocation
        result_file="$result_file ${invocation} ${error}"
    done < $RES_DIR/${codeletName}.invocations
    #if an error has occured, let's got to the next codelet
    if [[ ! ( -z ${err} ) ]]; then
        unset err
        rm -f $RES_DIR/${codeletName}.invocations
        rm -f $RES_DIR/${codeletName/__invivo__/__extracted__}_*.csv
        continue
    fi
    echo "$result_file" >> $RES_DIR/invocation_error
    #Compute error between invivo and in vitro
    cy=`grep -F "${codeletName}," $RES_DIR/all_loops.csv | head -n 1 | cut -d ',' -f 3 | tr -d $'\r'`
    cycles=`echo "scale=3;${cycles}" | bc`
    codelet_part=`echo "scale=3;${cy}/${app_cycles}" | bc | awk '{printf "%f", $0}'`

    compute_error ${cycles} ${cy}

    if [[ $error > 0.15 ]]; then
        echo "NOT MATCHING: In vitro = $cycles & invivo = $cy (error = $error, exec = $codelet_part)"
    else
        echo "MATCHING: In vitro = $cycles & invivo = $cy (error = $error, exec = $codelet_part)"
        echo ${codeletName/__invivo__/} >> $RES_DIR/new_matching_codelets
    fi
    echo "$codeletName,$error,$codelet_part" >> $RES_DIR/matching_error.csv
    rm $RES_DIR/${codeletName}.invocations
    rm $RES_DIR/${codeletName/__invivo__/__extracted__}_*.csv
done < ${FILE}

#Plot for each codelets the error between invivo and invitro
if [[ ( -f  matching_error ) ]]; then
    $ROOT/density_error.R $RES_DIR/matching_error
fi
#compute matching and compare old and new method
$PROJECT_ROOT/src/granularity/compute_matching.R ./$RES_DIR
CYCLES=`cat $RES_DIR/app_cycles.csv | tail -n 1 | cut -d ',' -f 3`
echo "Before:"
$PROJECT_ROOT/src/granularity/granularity.py $RES_DIR/all_loops.csv --matching=$RES_DIR/matching_codelets ${CYCLES}
echo "After:"
$PROJECT_ROOT/src/granularity/granularity.py $RES_DIR/all_loops.csv --matching=$RES_DIR/new_matching_codelets ${CYCLES}
