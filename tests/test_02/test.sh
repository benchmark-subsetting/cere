#!/bin/bash
./IS > /dev/null 2> ./out 

grep "rank_for.cond" out > /dev/null
exit $?
