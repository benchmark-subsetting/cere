#!/bin/bash

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $ROOT/

TSTATUS=0

info() {
    echo "$(tput setaf 2)[  OK  ]$(tput sgr0) $TNAME $1"
}

error() {
    echo "$(tput setaf 3)[ FAIL ]$(tput sgr0) $TNAME $1"
    TSTATUS=1
}

for TFILE in tests/*.c; 
do
   cd $ROOT/
   TNAME=$(basename $TFILE)
   # Copy test file to root dir
   cp $TFILE $ROOT/test.c 

   # Make original program
   make original > /dev/null 2>&1 
   GOTSTATUS="$?"
   if [ "$GOTSTATUS" != "0" ] ; then
       error "failed: could not build original"
       continue
   fi

   # Run original on dump mode
   $ROOT/original 1 > /tmp/mem.dump.out
   GOTSTATUS="$?"
   if [ "$GOTSTATUS" != "0" ] ; then
       error "failed: original dump failed"
       continue
   fi

   cat /tmp/mem.dump.out | grep 'Checksum' > /tmp/mem.dump.checksum.exp

   # Make patched program
   make patched > /dev/null 2>&1
   GOTSTATUS="$?"
   if [ "$GOTSTATUS" != "0" ] ; then
       error "failed: could not build patched"
       continue
   fi

   # Run patched on replay mode
   $ROOT/patched 2 > /tmp/mem.dump.out
   GOTSTATUS="$?"
   if [ "$GOTSTATUS" != "0" ] ; then
       error "failed: could not replay"
       continue
   fi

   cat /tmp/mem.dump.out | grep 'Checksum' > /tmp/mem.dump.checksum.got

   diff /tmp/mem.dump.checksum.got /tmp/mem.dump.checksum.exp
   GOTSTATUS="$?"
   if [ "$GOTSTATUS" != "0" ] ; then
       error "failed: checksum do not match"
       continue
   fi
   info "passed"
   make clean > /dev/null 2>&1
done

exit $TSTATUS
