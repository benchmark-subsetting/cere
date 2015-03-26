#!/bin/bash

function do_test()
{
    make veryclean
    rm -rf cere_dumps/ *.ll
    echo "__cere__fft3d_swarztrauber__27" > loops
    python ../../cere configure --build_cmd="make -j4" --run_cmd="./FT"
    python ../../cere test --regions=loops
    # ensure cluster output exist
    test -e cere_measures/__cere__fft3d_swarztrauber__27.invocation && \
        test -e cere_measures/plots/__cere__fft3d_swarztrauber__27_byPhase.png
}


source ../source.sh
