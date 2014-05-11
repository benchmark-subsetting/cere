#!/bin/bash

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $ROOT/

TSTATUS=0

info() {
    echo "[  OK  ] $TNAME $1"
}

error() {
    echo "[ FAIL ] $TNAME $1"
    TSTATUS=1
}

for TDIR in test_*; 
do
   cd $ROOT/$TDIR

   ./test.sh
   GOTSTATUS="$?"
   if [ "$GOTSTATUS" != "0" ] ; then
       error "$TDIR failed"
       continue
   fi
   info "$TDIR passed"
done

exit $TSTATUS
