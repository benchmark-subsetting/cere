#!/usr/bin/env python

import sys
import os
import cPickle as pickle
import networkx as nx
from common.graph_utils import load_graph
import cere_configure
import logging
import csv
from pulp import LpInteger, LpMinimize, LpProblem, LpStatus, LpVariable, lpSum, GLPK

#~ ROOT = os.path.dirname(os.path.realpath(__file__))
#~ ERROR_TABLE_FILENAME = "{0}/table_error.csv".format(cere_configure.cere_config["cere_measures_path"])

#~ class Error_table:
    #~ def __init__(self):
        #~ self.table = []
#~ 
    #~ def complete_error_table(self, error, chosen, graph):
        #~ coverage = 0
        #~ for codelet in chosen:
            #~ coverage = coverage + graph.node[codelet]['_coverage']
        #~ self.table = self.table + [[error,coverage]]
#~ 
    #~ def write_table(self):
        #~ output = open(ERROR_TABLE_FILENAME,'w')
        #~ output.write("Error,Exec Time\n")
        #~ for c in self.table:
            #~ output.write(str(c[0]) + "," + str(c[1]) + "\n")

class Unsolvable(Exception):
    pass

def solve(graph, max_coverage=100, step=5):
    coverage = max_coverage
    while(coverage > 0):
        try:
            s = list(solve_under_coverage(graph, coverage))
            return s, coverage
        except Unsolvable:
            coverage = coverage - step
    
    raise Unsolvable()

def solve_under_coverage(graph, min_coverage=80):

    prob = LpProblem("granularity selection", LpMinimize)
    codelet_vars = LpVariable.dicts("codelet",
            graph,
            lowBound=0,
            upBound=1,
            cat=LpInteger)

    # Objective function:
    prob += lpSum([codelet_vars[n]*d['_coverage'] for n,d in graph.nodes(data=True)])

    # and with good coverage
    prob += (lpSum([codelet_vars[n]*d['_coverage'] for n,d in graph.nodes(data=True)]) >= min_coverage)

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
    with open("{0}/selected_codelets".format(cere_configure.cere_config["cere_measures_path"]), 'w') as output:
        # print header
        output.write("Id,Codelet Name,Selected,ParentId\n")
        # find roots
        for n, d in graph.nodes(data=True):
            if not graph.predecessors(n): #root
                output_codelet(output, graph, chosen, n, "None", set())

def solve_with_best_granularity(error):
    graph = load_graph()
    if graph == None:
        logging.critical("Granularity: Can't load graph")
        return False

    if( len(graph.nodes()) == 0):
        logging.info('Graph is empty')
        return True

    padding = max([len(d['_name']) for n,d in graph.nodes(data=True)])

    #~ table = Error_table()
    target_error_chosen = set()
    try:
        chosen, coverage = solve(graph)

        #~ table.complete_error_table(error, chosen, graph)
        print >>sys.stderr, "Solved with coverage >= %s" % coverage
        target_error_chosen = chosen
    except(Unsolvable):
        print >>sys.stderr, "Solution impossible"
        #~ table.complete_error_table(error, set(), graph)

    output_tree(graph, target_error_chosen)
    #~ table.write_table()
    for c in target_error_chosen:
        print >>sys.stderr, "> {0} {1}".format(graph.node[c]['_name'].ljust(padding), graph.node[c]['_coverage'])
    return True
