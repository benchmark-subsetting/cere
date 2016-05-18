#!/usr/bin/env python
import os
import sys

if len(sys.argv) != 2:
    print("usage: {0} DIRECTORY".format(sys.argv[0]))
    exit(1)

os.chdir(sys.argv[1])

dumps = []

for f in os.listdir("."):
    if f.endswith(".memdump"):
        dumps.append(int(f.split(".")[0], 16))

dumps.sort()

def consecutive(n,m):
    return abs(dumps[n]-dumps[m]) == 4096

ranges = []

start = 0
last = 0
for i in range(1, len(dumps)):
    if consecutive(last, i):
        last = i
    else:
        ranges.append((start, last))
        start = i
        last = i

ranges.append((start, last))

#print(map(hex, dumps))


for ran in ranges:
    start, stop = ran
    with open("{0:012x}.memdump".format(dumps[start]), "ab") as first:
        for i in range(start+1, stop+1):
            other_name = "{0:012x}.memdump".format(dumps[i])
            with open(other_name, "rb") as other:
                first.write(other.read(4096))
            os.remove(other_name)

print(ranges)

print("Compressing done!")
