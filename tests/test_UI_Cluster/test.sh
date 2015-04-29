#!/bin/bash

function do_test()
{
    make veryclean
    echo "__cere__fft3d_swarztrauber__27" > loops
    python ../../cere configure --build_cmd="make -j4" --run_cmd="./FT"
    python ../../cere test --regions=loops
    # ensure cluster output exist
    test -e .cere/traces/__cere__fft3d_swarztrauber__27.invocations && \
        test -e .cere/plots/__cere__fft3d_swarztrauber__27_byPhase.png
}


source ../source.sh
