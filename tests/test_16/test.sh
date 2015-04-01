#!/bin/bash
set -e

function do_test()
{
     ../../cere/cere_test/clusterize_invocations.py __invivo__randi8_vranlc__69 cere_measures/__invivo__randi8_vranlc__69.csv cere_measures/__invivo__randi8_vranlc__69.bin
    exit 0
}

source ../source.sh
