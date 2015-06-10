#!/bin/bash

function do_test()
{
    ../../cere configure --build-cmd="" --run-cmd=""
    python test.py
}

source ../source.sh
