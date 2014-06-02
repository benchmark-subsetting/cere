#!/bin/bash

function do_test()
{
make veryclean > /dev/null 2>&1
make test_dump > /dev/null 2>&1
LD_BIND_NOW=1 ./test_dump > $TMPDIR/test.dump.out
cat $TMPDIR/test.dump.out

make test_replay
./test_replay > $TMPDIR/test.replay.out
cat $TMPDIR/test.replay.out

cat $TMPDIR/test.dump.out | grep "value =" | head -n1 > $TMPDIR/test.a
cat $TMPDIR/test.replay.out | grep "value =" | head -n1 > $TMPDIR/test.b

diff -u $TMPDIR/test.a $TMPDIR/test.b
}

source ../source.sh
