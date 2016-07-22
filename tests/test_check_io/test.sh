#!/bin/bash

function do_test()
{
    rm -rf .cere
    cere configure --build-cmd make --run-cmd ./test --clean-cmd "make clean"
    cere regions

    for region in $(cat regions.csv |grep __cere__|cut -d, -f1); do
        cere capture --region="$region" --invocation 1
    done

    if grep "__cere__test_main_15" .cere/replays/invalid_regions ; then 
        echo ok	
    else
	return 1
    fi

    if grep "__cere__test_main_9" .cere/replays/invalid_regions ; then 
        echo ok	
    else
	return 1
    fi

    # only the last region is valid
    if grep "__cere__test_main_20" .cere/replays/invalid_regions; then
	return 1	
    else
	echo ok
    fi
}

source ../source.sh
