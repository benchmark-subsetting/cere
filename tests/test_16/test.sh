#!/bin/bash
set -e

function do_test()
{
    cere configure --run-cmd="" --build-cmd="" --clean-cmd=""
    cere selectinv --region=__cere__randi8_vranlc__69
    exit $?
}

source ../source.sh
