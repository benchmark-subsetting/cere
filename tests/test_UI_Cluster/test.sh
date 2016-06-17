#!/bin/bash

function do_test()
{
    make veryclean
    echo "__cere__fft3d_swarztrauber__27" > loops
    cere configure --build-cmd="make -j4" --clean-cmd="make clean" --run-cmd="./FT"
    cere check-matching --regions-file=loops
    # ensure cluster output exist
    test -e .cere/traces/__cere__fft3d_swarztrauber__27.invocations && \
        test -e .cere/plots/__cere__fft3d_swarztrauber__27_byPhase.png
}


source ../source.sh
