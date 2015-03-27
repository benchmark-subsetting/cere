#!/usr/bin/env python

import sys
import os
import cPickle as pickle
import networkx as nx
from common.graph_utils import load_graph
import cere_configure
import update_graph
import logging
import csv

class Error_table:
    def __init__(self):
        self.table = []

    def complete_error_table(self, error, coverage):
        self.table = self.table + [[error,coverage]]

    def write_table(self, error_file):
        output = open(error_file,'w')
        output.write("Error,Exec Time\n")
        for c in self.table:
            output.write(str(c[0]) + "," + str(c[1]) + "\n")

def solve(graph):
    coverage = 0
    nodes = []
    for n, d in graph.nodes(data=True):
        if d['_matching']:
            coverage = coverage + d['_self_coverage']
            nodes.append(n)
    return nodes, coverage

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

def solve_with_best_granularity(args):
    graph = load_graph()
    if graph == None:
        logging.critical("Granularity: Can't load graph")
        return False

    if( len(graph.nodes()) == 0):
        logging.info('Graph is empty')
        return True

    #Compute coverage for different error
    #~ error_filename = "{0}/table_error.csv".format(cere_configure.cere_config["cere_measures_path"])
    #~ table = Error_table()
    #~ args.max_error = 100
    #~ while args.max_error >= 5:
        #~ update_graph.update(args)
        #~ graph = load_graph()
        #~ table_chosen, table_coverage = solve(graph)
        #~ table.complete_error_table(args.max_error, table_coverage)
        #~ args.max_error = args.max_error-5
    #~ table.write_table(error_filename)

    #~ args.max_error = 15
    #~ update_graph.update(args)
    graph = load_graph()
    padding = max([len(d['_name']) for n,d in graph.nodes(data=True)])
    chosen, coverage = solve(graph)
    output_tree(graph, chosen)

    if coverage == 0:
        print >>sys.stderr, "Solution impossible"
    else:
        print >>sys.stderr, "Solved with coverage >= %s" % coverage

    for c in chosen:
        print >>sys.stderr, "> {0} {1}".format(graph.node[c]['_name'].ljust(padding), graph.node[c]['_self_coverage'])
    return True
