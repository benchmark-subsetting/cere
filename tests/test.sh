#!/bin/sh
RANDOMIZE_SPACE=$(cat /proc/sys/kernel/randomize_va_space)
if test "$RANDOMIZE_SPACE" = "0"; then
    echo "layout space randomization is required during replay"
    echo "please type echo 1 | sudo tee /proc/sys/kernel/randomize_va_space"
    echo "before running tests"
    exit 127
fi
../autoconf/tap-driver.sh --test-name testplan.test --log-file testplan.log --trs-file testplan.trs --color-tests yes -- ./testplan.test
grep ":global-test-result: PASS" testplan.trs > /dev/null
exit $?
