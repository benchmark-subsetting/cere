#!/bin/bash
./BT > /dev/null 2> ./out 

grep "exact_solution__3" out > /dev/null
exit $?
