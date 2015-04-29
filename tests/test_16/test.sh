#!/bin/bash
set -e

function do_test()
{
    python ../../cere configure --run_cmd="" --build_cmd=""
    python ../../cere selectinv --region=__cere__randi8_vranlc__69
    exit $?
}

source ../source.sh
