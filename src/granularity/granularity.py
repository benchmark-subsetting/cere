#!/usr/bin/env python
import argparse
import csv
import sys
import os
from pulp import LpInteger, LpMinimize, LpProblem, LpStatus, LpVariable, lpSum, GLPK

ROOT = os.path.dirname(os.path.realpath(__file__))
tolerated_error = [5,10,15,20,25,30,99.9]
ERROR = 15
ERROR_TABLE_FILENAME = "measures/table_error.csv"

class Codelet_matching:
    def __init__(self,matching_file):
        self.dict = {}
        loops = csv.DictReader(matching_file, delimiter=',')
        for loop in loops:
            self.dict[(loop["Codelet Name"],"error")] = loop["Error"]
            self.dict[(loop["Codelet Name"],"exec time")] = loop["Exec Time"]

class Error_table:
    def __init__(self):
        self.table = []

    def complete_error_table(self,error_min,prec_coverage_max,chosen, matching):
        temp = []
        coverage = 0
        myList = []
        for codelet in chosen:
            myList = myList + [float(matching.dict[(codelet.name,"error")])]
        for j in [i[0] for i in sorted(enumerate(myList), key=lambda x:x[1])]:
            codelet = chosen[j]
            name = codelet.name
            time = float(matching.dict[(name,"exec time")])*100
            error = float(matching.dict[(name,"error")])*100
            index = 0
            if(error < error_min):
                coverage = coverage + time
            else:
                cov = coverage
                for i in range(len(temp)):
                    if (index == 0):
                        if(error<temp[i][0]):
                            index = i
                            temp[i][1] = temp[i][1] + time
                        else:
                            cov = coverage + temp[i][1]
                    else:
                        temp[i][1] = temp[i][1] + time
                if(index == 0):
                    if ((cov + time)>prec_coverage_max):
                        temp.append([error,cov + time])
                else:
                    temp.insert(i,[error,cov + time])
        if(len(temp) == 0):
            return prec_coverage_max
        self.table = self.table + temp
        return temp[-1][1]

    def write_table(self):
        output = open(ERROR_TABLE_FILENAME,'w')
        output.write("Error,Exec Time\n")
        for c in self.table:
            output.write(str(c[0]) + "," + str(c[1]) + "\n")


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
        max_coverage=0.95, matching_loops=None, error=ERROR):
    if(matching_loops):
        # By default, codelets are considered matching
        for c in codelets:
            c.matching = True
            if not matches(matching_loops, c,error):
                c.matching = False
    coverage = max_coverage
    while(coverage > 0):
        try:
            s = list(solve_under_coverage(codelets, appli_cycles, min_cycles,
                coverage))
            if (error == ERROR):
                print >>sys.stderr, "Solved with coverage >= %s" % coverage
            return s
        except Unsolvable:
            coverage = coverage - step
    
    raise Unsolvable()

def matches(matching, c, error):
    if (((c.name,"error") in matching.dict.keys()) and ((float(matching.dict[(c.name,"error")])*100) <= error)):
        return True
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
        if args.matching:
            matching_loops = Codelet_matching(args.matching)
        error_prec = 0
        coverage_max = 0
        table = Error_table()
        for error in tolerated_error:
            try:
                chosen = solve(codelets.values(),
                               appli_cycles=args.application_cycles,
                               min_cycles=args.min_cycles,
                               matching_loops=matching_loops,error=error)

                coverage_max = table.complete_error_table(error_prec,coverage_max,chosen,matching_loops)
                if (error == ERROR):
                    output_tree(args.o, codelets, chosen)
            except(Unsolvable):
                if(error == ERROR):
                    print >>sys.stderr, "Solution impossible"
                    sys.exit(1)
            error_prec = error

        table.write_table()
        print >>sys.stderr, "===== Solution with coverage ====="

        for c in chosen:
            print >>sys.stderr, "> {} {}".format(c.name.ljust(padding), round((c.cycles*c.callcount)/args.application_cycles*100, 2))
