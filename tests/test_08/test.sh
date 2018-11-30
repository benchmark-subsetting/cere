#!/bin/bash

function do_test()
{
# disabling test, since newer dragonegg versions cannot compile linpck.ll, this is not a bug with CERE,
#
#/usr/lib/llvm-3.4/bin/opt: linpck.ll:1946:69: error: referenced value is not a basic block
#  %6 = phi i8* [ %151, %"35.5_crit_edge" ], [ blockaddress(@snrm2_, %"9"), %newFuncRoot ], !dbg !177
#                                                                    ^
#linpck.ll:1946:69: error: referenced value is not a basic block
#  %6 = phi i8* [ %151, %"35.5_crit_edge" ], [ blockaddress(@snrm2_, %"9"), %newFuncRoot ], !dbg !177
#
echo "disabled"
#    rm -rf .cere/ *.ll
#    make clean
#    make -j4 CERE_MODE="dump --region=__cere__advx1_advx1__622 --invocation=1"
#    ./zeusmp
#
#    make clean
#    make -j4 CERE_REPLAY_REPETITIONS=1 CERE_MODE="replay --region=__cere__advx1_advx1__622 --invocation=1 --instrument --wrapper=-lcere_rdtsc"
#    ./zeusmp > $TMPDIR/test.replay.out 2>&1
}

source ../source.sh
