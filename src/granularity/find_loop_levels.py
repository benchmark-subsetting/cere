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
    rdtscCSV = open(sys.argv[1], "rb")
    rdtscReader = csv.reader(rdtscCSV, delimiter=',')
    rdtscReader.next() #skip header

    max_level = {}
    levels = {}

    for row in rdtscReader:
        regionName = row[0]
        baseName = regionName.split("#")[-1]
        level = regionName.count("#")

        if baseName in max_level:
            max_level[baseName] = max(max_level[baseName], level)
        else:
            max_level[baseName] = level

    for name, level in max_level.iteritems():
        if level not in levels: levels[level] = []
        levels[level].append(name)

    dump_to_files(levels)
