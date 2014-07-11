#!/usr/bin/env python
import argparse
import csv
import sys
import os
from pulp import LpInteger, LpMinimize, LpProblem, LpStatus, LpVariable, lpSum, GLPK

class Codelet:
    def __init__(self, name, callcount, totalcycles):
        self.fullName = name
        self.name = name.split("#")[-1]
        self.depth = self.name.count("#")
        self.callcount = callcount
        # we store cycles per iteration
        self.cycles = float(totalcycles)/callcount
        self.matching = True
        self.direct_parents = set()
        self.parents = set()
        self.children = set()
        self.add_parents(self.fullName)

    def __repr__(self):
        return str(id(self))

    def add_parents(self, fullName):
        parents = fullName.split("#")[:-1]
        if parents:
            self.direct_parents.add(parents[-1])
        for parent in parents:
            self.parents.add(parent)

    def add_children(self, name):
        self.children.add(name)

    def is_children_of(self, other):
        return other.name in self.parents

def parse(csvfile):
    csvreader = csv.reader(csvfile, delimiter=',')
    csvreader.next() # skip header
    codelets = {}
    for row in csvreader:
        c = Codelet(row[0], int(row[1]), int(row[2]))
        if c.name not in codelets:
            codelets[c.name] = c
        else:
            assert codelets[c.name].cycles == c.cycles
            assert codelets[c.name].callcount == c.callcount
            codelets[c.name].add_parents(c.fullName)

    for name,codelet in codelets.iteritems():
        for direct_parent in codelet.direct_parents:
            codelets[direct_parent].add_children(name)

    csvfile.close()
    return codelets

class Unsolvable(Exception):
    pass

def solve(codelets, appli_cycles, min_cycles=10**6, step=0.05,
        max_coverage=0.95, matching_file=None):

    if matching_file:
        matching_loops = [line[:-1] for line in matching_file.readlines()]

        # By default, codelets are considered matching
        for c in codelets:
            if not matches(matching_loops, c):
                c.matching = False

    coverage = max_coverage
    while(coverage > 0):
        try:
            s = list(solve_under_coverage(codelets, appli_cycles, min_cycles,
                coverage))
            print >>sys.stderr, "Solved with coverage >= %s" % coverage
            return s
        except Unsolvable:
            coverage = coverage - step

    print >>sys.stderr, "Solution impossible"
    sys.exit(1)

def matches(matching, c):
    for m in matching:
        if c.name.endswith(m): return True
    return False

def solve_under_coverage(codelets, appli_cycles, min_cycles=10**6,
        min_coverage=0.80):

    prob = LpProblem("granularity selection", LpMinimize)
    codelet_vars = LpVariable.dicts("codelet",
            codelets,
            lowBound=0,
            upBound=1,
            cat=LpInteger)

    # Objective function: the codelet total cost must be minimal
    # we want small codelets
    prob += lpSum([codelet_vars[c]*c.cycles for c in codelets])

    # But not too small, we require at least threshold cycles for chosen
    # codelets
    for c in codelets:
        prob += (min_cycles*codelet_vars[c] <= c.cycles*c.callcount)

    # and with good coverage
    prob += (lpSum([codelet_vars[c]*c.cycles*c.callcount for c in codelets]) >= min_coverage * appli_cycles)

    # selected codelets should match
    for c in codelets:
        if not c.matching:
            prob += codelet_vars[c] == 0

    # Finally we should never include both the children and the parents
    for dad in codelets:
        for son in codelets:
            # XXX this can be implemented much more efficiently,
            # by using the transitivity of being a parent
            if not son.is_children_of(dad): continue

            # We cannot select dad and son at the same time
            prob += codelet_vars[dad] + codelet_vars[son] <= 1

    #prob.solve(GLPK())
    prob.solve()
    if (LpStatus[prob.status] != 'Optimal'):
        raise Unsolvable()

    for v in prob.variables():
        assert v.varValue == 1.0 or v.varValue == 0.0
        if v.varValue == 1.0:

            for c in codelets:
                if ("codelet_"+repr(c)) == v.name:
                    yield c

unique_id = 0

def get_uid():
    global unique_id
    unique_id += 1
    return str(unique_id)


def output_codelet(output, codelets, chosen, codelet, direct_parent_id):
    selected = "true" if codelet in chosen else "false"
    codelet_id =  get_uid()
    output.write(",".join([codelet_id, codelet.name, selected, direct_parent_id]) + "\n")
    for child_name in codelet.children:
        output_codelet(output, codelets, chosen, codelets[child_name], codelet_id)

def output_tree(output, codelets, chosen):
    # print header
    output.write("Id,Codelet Name,Selected,ParentId\n")
    # find roots
    for codelet in codelets.itervalues():
        if not codelet.parents:
            output_codelet(output, codelets, chosen, codelet, "None")

    output.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Choose a subset of codelets", prog="granularity.py")
    subparsers = parser.add_subparsers(help="ACTION", dest="mode")
    reduce_parser = subparsers.add_parser("reduce", help="find optimal codelet granularity")
    reduce_parser.add_argument('profile_file', type=file)
    reduce_parser.add_argument('application_cycles', type=int)
    reduce_parser.add_argument('--min_cycles', type=int, default=10**6)
    reduce_parser.add_argument('--matching', type=file, default=None)
    reduce_parser.add_argument('-o', type=argparse.FileType('w'), required=True, help="output file", metavar="output_file")

    filter_parser = subparsers.add_parser("filter", help="filter codelets < 1% execution time")
    filter_parser.add_argument('profile_file', type=file)
    filter_parser.add_argument('application_cycles', type=int)
    filter_parser.add_argument('-o', type=argparse.FileType('w'), required=True, help="output file", metavar="output_file")


    args = parser.parse_args()

    codelets = parse(args.profile_file)

    padding = max([len(c.name) for c in codelets.itervalues()])

    if args.mode == "filter":
        for c in codelets.itervalues():
            print >>sys.stderr, "# {} {}".format(c.name.ljust(padding), round((c.cycles*c.callcount)/args.application_cycles*100, 2))
            if((c.cycles*c.callcount)/args.application_cycles*100) >= 1:
                args.o.write(c.name + '\n')
        args.o.close()
    else:
        chosen = solve(codelets.values(),
                appli_cycles=args.application_cycles,
                min_cycles=args.min_cycles,
                matching_file=args.matching)

        output_tree(args.o, codelets, chosen)

        print >>sys.stderr, "===== Solution with coverage ====="

        for c in chosen:
            print >>sys.stderr, "> {} {}".format(c.name.ljust(padding), round((c.cycles*c.callcount)/args.application_cycles*100, 2))
