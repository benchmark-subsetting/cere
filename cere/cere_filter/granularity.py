#!/usr/bin/env python

#~ import argparse
#~ import csv
import sys
import os
import cPickle as pickle
import networkx as nx
from graph_utils import load_graph
import cere_configure
import logging
from pulp import LpInteger, LpMinimize, LpProblem, LpStatus, LpVariable, lpSum, GLPK

ROOT = os.path.dirname(os.path.realpath(__file__))
#~ tolerated_error = [5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,99.9]
#~ TARGET_ERROR = 15
#~ assert(TARGET_ERROR in tolerated_error)
ERROR_TABLE_FILENAME = "{0}/table_error.csv".format(cere_configure.cere_config["cere_measures_path"])

#~ class Codelet_matching(dict):
    #~ def __init__(self,matching_file):
        #~ loops = csv.DictReader(matching_file, delimiter=',')
        #~ for loop in loops:
            #~ self[(loop["Codelet Name"],"error")] = loop["Error"]
            #~ self[(loop["Codelet Name"],"exec time")] = loop["Exec Time"]


class Error_table:
    def __init__(self):
        self.table = []

    def complete_error_table(self, error, chosen, graph):
        coverage = 0
        for codelet in chosen:
            coverage = coverage + graph.node[codelet]['_self_coverage']
        self.table = self.table + [[error,coverage]]

    def write_table(self):
        output = open(ERROR_TABLE_FILENAME,'w')
        output.write("Error,Exec Time\n")
        for c in self.table:
            output.write(str(c[0]) + "," + str(c[1]) + "\n")


#~ class Codelet:
    #~ def __init__(self, name, callcount, totalcycles):
        #~ self.fullName = name
        #~ self.name = name.split("#")[-1]
        #~ self.depth = self.fullName.count("#")
        #~ self.callcount = callcount
        #~ # we store cycles per iteration
        #~ self.cycles = float(totalcycles)/callcount
        #~ self.matching = True
        #~ self.direct_parents = set()
        #~ self.parents = set()
        #~ self.children = set()
        #~ self.add_parents(self.fullName)
#~ 
    #~ def __repr__(self):
        #~ return str(id(self))
#~ 
    #~ def add_parents(self, fullName):
        #~ parents = fullName.split("#")[:-1]
        #~ if parents:
            #~ self.direct_parents.add(parents[-1])
        #~ for parent in parents:
            #~ self.parents.add(parent)
#~ 
    #~ def add_children(self, name):
        #~ self.children.add(name)
#~ 
    #~ def is_children_of(self, other):
        #~ return other.name in self.parents
    #~ 
    #~ def update_depth(self, new_depth):
        #~ if new_depth < self.depth:
            #~ self.depth = new_depth

#~ def parse(csvfile):
    #~ csvreader = csv.reader(csvfile, delimiter=',')
    #~ csvreader.next() # skip header
    #~ codelets = {}
    #~ for row in csvreader:
        #~ c = Codelet(row[0], int(row[1]), int(row[2]))
        #~ if c.name not in codelets:
            #~ codelets[c.name] = c
        #~ else:
            #~ assert codelets[c.name].cycles == c.cycles
            #~ assert codelets[c.name].callcount == c.callcount
            #~ codelets[c.name].update_depth(c.depth)
            #~ codelets[c.name].add_parents(c.fullName)
#~ 
    #~ for name,codelet in codelets.iteritems():
        #~ for direct_parent in codelet.direct_parents:
            #~ if direct_parent in codelets:
                #~ codelets[direct_parent].add_children(name)
#~ 
    #~ csvfile.close()
    #~ return codelets

class Unsolvable(Exception):
    pass

def solve(graph, max_coverage=100, step=5):
    #~ if(matching_loops):
        #~ # By default, codelets are considered matching
        #~ for c in codelets:
            #~ c.matching = True
            #~ if not matches(matching_loops, c,error):
                #~ c.matching = False
    coverage = max_coverage
    while(coverage > 0):
        try:
            s = list(solve_under_coverage(graph, coverage))
            return s, coverage
        except Unsolvable:
            coverage = coverage - step
    
    raise Unsolvable()

#~ def matches(matching, c, error):
    #~ if (((c.name,"error") in matching.keys()) and ((float(matching[(c.name,"error")])*100) <= error)):
        #~ return True
    #~ return False

def solve_under_coverage(graph, min_coverage=80):

    prob = LpProblem("granularity selection", LpMinimize)
    codelet_vars = LpVariable.dicts("codelet",
            graph,
            lowBound=0,
            upBound=1,
            cat=LpInteger)

    # Objective function:
    prob += lpSum([codelet_vars[n]*d['_self_coverage'] for n,d in graph.nodes(data=True)])

    # and with good coverage
    prob += (lpSum([codelet_vars[n]*d['_self_coverage'] for n,d in graph.nodes(data=True)]) >= min_coverage)

    # selected codelets should match
    for n,d in graph.nodes(data=True):
        if not d['_matching']:
            prob += codelet_vars[n] == 0

    # Finally we should never include both the children and the parents
    for node in graph.nodes():
        for predecessor in graph.predecessors(node):
            # We cannot select dad and son at the same time
            prob += codelet_vars[node] + codelet_vars[predecessor] <= 1

    #prob.solve(GLPK())
    prob.solve()
    if (LpStatus[prob.status] != 'Optimal'):
        raise Unsolvable()

    for v in prob.variables():
        assert v.varValue == 1.0 or v.varValue == 0.0
        if v.varValue == 1.0:

            for n,d in graph.nodes(data=True):
                if ("codelet_"+str(n)) == v.name:
                    yield n

unique_id = 0

def get_uid():
    global unique_id
    unique_id += 1
    return str(unique_id)


def output_codelet(output, graph, chosen, node, direct_parent_id, parents):
    selected = "true" if node in chosen else "false"
    codelet_id =  get_uid()
    output.write(",".join([codelet_id, graph.node[node]['_name'], selected, direct_parent_id]) + "\n")
    for child_name in graph.successors(node):
        if child_name not in parents:
            output_codelet(output, graph, chosen, child_name, codelet_id, parents | set([node]))

def output_tree(graph, chosen):
    with open("{0}/selected_codelets".format(cere_configure.measures_path), 'w') as output:
        # print header
        output.write("Id,Codelet Name,Selected,ParentId\n")
        # find roots
        for n in graph.nodes():
            if not graph.predecessors(n): #root
                output_codelet(output, graph, chosen, n, "None", set())

def solve_with_best_granularity(error):
    graph = load_graph()
    if graph == None:
        print("Error: Can't load graph")
        return False

    #~ codelets = parse(args.profile_file)

    padding = max([len(d['_name']) for n,d in graph.nodes(data=True)])

    #~ if args.matching:
        #~ matching_loops = Codelet_matching('measures/matching_error.csv')
    #~ else:
        #~ matching_loops = None
    table = Error_table()
    target_error_chosen = set()
    try:
        chosen, coverage = solve(graph)

        table.complete_error_table(error, chosen, graph)
        #~ if (error == TARGET_ERROR):
        print >>sys.stderr, "Solved with coverage >= %s" % coverage
        target_error_chosen = chosen
    except(Unsolvable):
        #~ if(error == TARGET_ERROR):
        print >>sys.stderr, "Solution impossible"
        table.complete_error_table(error, set(), graph)

    output_tree(graph, target_error_chosen)
    table.write_table()
    #~ print >>sys.stderr, "===== Solution with coverage ====="
    for c in target_error_chosen:
        print >>sys.stderr, "> {0} {1}".format(graph.node[c]['_name'].ljust(padding), graph.node[c]['_self_coverage'])
    return True
