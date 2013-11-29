#!/usr/bin/env python

import sys, csv, os

def usage():
    print("Usage: ./fusion.py all_loop.csv level_loop.csv")

def get_loops(csvfile):
    loops = {}
    loopCSV = open(csvfile, "rb")
    loopReader = csv.reader(loopCSV, delimiter=',')
    loopReader.next() #skip header
    for row in loopReader:
        loops[row[0]] = {}
        loops[row[0]]["Call Count"] = row[1]
        loops[row[0]]["CPU_CLK_UNHALTED_CORE"] = row[2]
    return loops

if __name__ == "__main__":
    if len(sys.argv) != 3:
        usage()
        sys.exit(1)
    if not os.path.isfile(sys.argv[1]) or not os.path.isfile(sys.argv[2]):
        print("Cannot open rdtsc file")
        sys.exit(1)
    allLoop = get_loops(sys.argv[1])
    loopPerLevel = get_loops(sys.argv[2])

    allLoopCSV = open(sys.argv[1], "w")
    allLoopWriter = csv.writer(allLoopCSV, delimiter=',')
    allLoopWriter.writerow(["Codelet Name","Call Count","CPU_CLK_UNHALTED_CORE"])

    for codeletName, codeletData in allLoop.items():
        noLevelName = codeletName.split("#")[-1]
        if noLevelName in loopPerLevel:
            allLoop[codeletName]["Call Count"] = loopPerLevel[noLevelName]["Call Count"]
            allLoop[codeletName]["CPU_CLK_UNHALTED_CORE"] = loopPerLevel[noLevelName]["CPU_CLK_UNHALTED_CORE"]
        allLoopWriter.writerow([codeletName, allLoop[codeletName]["Call Count"], allLoop[codeletName]["CPU_CLK_UNHALTED_CORE"]])
