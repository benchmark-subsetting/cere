#!/bin/bash

function do_test()
{
    python ../../cere configure --build_cmd="" --run_cmd=""
    python test.py
}

source ../source.sh
