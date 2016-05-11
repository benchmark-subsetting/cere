#!/bin/bash
make clean
rm -f .cere/replays/* cere.log realmain.c
../../cere flag --region __cere__fft3d_swarztrauber__27 --flags-file small_seq_1.csv -f

cat .cere/replays/regions_flags.csv
exit $?
