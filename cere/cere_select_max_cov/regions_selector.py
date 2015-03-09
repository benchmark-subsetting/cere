#!/usr/bin/env python

import sys
import os
import cPickle as pickle
import networkx as nx
from common.graph_utils import load_graph
import cere_configure
import logging
import csv

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

def solve_with_best_granularity(error):
    graph = load_graph()
    if graph == None:
        logging.critical("Granularity: Can't load graph")
        return False

    if( len(graph.nodes()) == 0):
        logging.info('Graph is empty')
        return True

    padding = max([len(d['_name']) for n,d in graph.nodes(data=True)])

    chosen, coverage = solve(graph)
    if coverage == 0:
        print >>sys.stderr, "Solution impossible"
    else:
        print >>sys.stderr, "Solved with coverage >= %s" % coverage
        

    output_tree(graph, chosen)
    for c in chosen:
        print >>sys.stderr, "> {0} {1}".format(graph.node[c]['_name'].ljust(padding), graph.node[c]['_self_coverage'])
    return True
