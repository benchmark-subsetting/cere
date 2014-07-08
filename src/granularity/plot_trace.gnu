#!/usr/bin/gnuplot

set term "png" size 1000,358
set output outputFile.".png"
set xlabel 'Invocation'
set ylabel 'Cycles'
plot for [file in filename] file binary format="%double" using 2:1 with points pt 7 ps 1 title file
