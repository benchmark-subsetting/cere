#!/bin/bash

make veryclean > /dev/null 2>&1
../../src/granularity/coverage.sh . "./bzip2 -k -z -f dryer.jpg" make
exit $?
