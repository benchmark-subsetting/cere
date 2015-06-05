#!/bin/bash
set -e

function do_test()
{
    ../../cere configure --run_cmd="" --build_cmd=""
    ../../cere selectinv --region=__cere__randi8_vranlc__69
    exit $?
}

source ../source.sh
