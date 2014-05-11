#!/bin/sh

TMPDIR=`mktemp -d`
make veryclean > /dev/null 2>&1 && make test_dump  > /dev/null 2>&1
./test_dump 2> $TMPDIR/test.dump.out

make test_replay > /dev/null 2>&1
LD_BIND_NOW=1 ./test_replay 2> $TMPDIR/test.replay.out

cat $TMPDIR/test.dump.out | grep "&a" | head -n1 > $TMPDIR/test.a
cat $TMPDIR/test.replay.out | grep "&a" | head -n1 > $TMPDIR/test.b

diff $TMPDIR/test.a $TMPDIR/test.b
rm -rf "$TMPDIR"
exit $?
