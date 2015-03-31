#!/bin/bash

DIRECTORY=$1

# Check Python
pep8 --first --exclude=libs/ $DIRECTORY 

# Check C/C++
TMPFILE=$(tempfile)
for file in $(find $DIRECTORY/src -name '*.[ch]' -or -name '*.cpp'); do 
    clang-format -style=LLVM $file > $TMPFILE
    diff -q $file $TMPFILE > /dev/null 
    if [ "$?" -ne "0" ]; then
        echo "$file does not conform to LLVM style"
    fi 
done

rm -f $TMPFILE

