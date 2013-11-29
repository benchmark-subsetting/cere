#!/usr/bin/env python

import sys, csv, os

def dump_to_files(levels):
    for lvl in levels:
        f = open("level_{0}".format(lvl), 'w')
        for codelet in levels[lvl]:
            f.write("{0}\n".format(codelet))
        f.close()

def usage():
    print("Usage: ./find_loop_level.py rdtsc_file.csv")
    print("rdtsc_file.csv: the file containing all codelets measure\n")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        usage()
        sys.exit(1)
    if not os.path.isfile(sys.argv[1]):
        print("Cannot open file {0}\n".format(sys.argv[1]))
        sys.exit(1)
    levels = {}
    rdtscCSV = open(sys.argv[1], "rb")
    rdtscReader = csv.reader(rdtscCSV, delimiter=',')
    rdtscReader.next() #skip header
    for row in rdtscReader:
        level = row[0].count("#")
        for x in range(level+1):
            if x not in levels:
                levels[x] = []
            codeletName = row[0].split("#")[x]
            if codeletName not in levels[x]:
                levels[x].append(codeletName)
    dump_to_files(levels)

