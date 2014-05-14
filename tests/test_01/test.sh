#!/bin/bash

TMPDIR=`mktemp -d`
make veryclean > /dev/null 2>&1
make test_dump > /dev/null 2>&1
LD_BIND_NOW=1 ./test_dump > $TMPDIR/test.dump.out
cat $TMPDIR/test.dump.out

make test_replay
./test_replay > $TMPDIR/test.replay.out
cat $TMPDIR/test.replay.out

cat $TMPDIR/test.dump.out | grep "&a" | head -n1 > $TMPDIR/test.a
cat $TMPDIR/test.replay.out | grep "&a" | head -n1 > $TMPDIR/test.b

diff $TMPDIR/test.a $TMPDIR/test.b
exit $?
