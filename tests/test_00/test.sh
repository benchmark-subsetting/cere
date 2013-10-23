#!/bin/sh
make veryclean > /dev/null 2>&1 && make test_dump  > /dev/null 2>&1
./test_dump 2> /tmp/test.dump.out
make test_replay > /dev/null 2>&1
./test_replay 2> /tmp/test.replay.out

cat /tmp/test.dump.out | grep "&a" | head -n1 > /tmp/test.a
cat /tmp/test.replay.out | grep "&a" | head -n1 > /tmp/test.b

diff /tmp/test.a /tmp/test.b
exit $?
