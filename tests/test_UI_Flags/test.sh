#!/bin/bash
make clean
rm -f .cere/replays/* cere.log realmain.c
cere flag --region __cere__fft3d_swarztrauber__27 -f small_seq_1.csv

cat .cere/flags/regions_flags.csv
exit $?
