#!/usr/bin/env python

import sys
import os
import csv
import cPickle as pickle
import networkx as nx
import subprocess
from graph_utils import *
import cere_configure
import logging

LIST_PREFIX = ["__invivo__","__extracted__"]

def read_csv(File):
    try:
        FILE = open(File, 'rb')
    except (IOError):
        return []
    Dict = csv.DictReader(FILE, delimiter=',')
    return Dict

def update_nodes(graph, lines, max_allowed_error):
    matching = {}
    for line in lines:
        matching[suppr_prefix(line["Codelet Name"])] = float(line["Error"])

    for region_name, error in matching.iteritems():
        #find the node in the graph
        for n,d in graph.nodes(data=True):
            if region_name in d['_name'] and not d['_tested']:
                d['_tested'] = True
                if error <= max_allowed_error:
                    d['_matching'] = True
                else:
                    d['_valid'] = False
                d['_error'] = float(error)
    return graph

def suppr_prefix(name):
    '''
    Remove prefix of a region
    '''
    for pre in LIST_PREFIX:
        name = name.replace(pre,"")
    return name

def update(binary_cmd, compile_cmd, error):
    logging.info("Start graph updating")
    graph = load_graph()
    if graph == None:
        logging.critical("No graph to load")
        return False

    step=0
    while(1):
        step = step + 1
        #1) Something new?
        lines = read_csv("{0}/matching_error.csv".format(cere_configure.default_measures_path))
        graph = update_nodes(graph, lines, error)

        #2) rewind self to parents for invalid loops
        nodes = (list(reversed(nx.topological_sort(graph))))
        for node in nodes:
            cancel = False
            #if it's an invalid node (not matching or not extracted) or if it's too small
            if not graph.node[node]['_valid'] or graph.node[node]['_small']:
                #if there is still a successor not tested, we do nothing.
                for successor in graph.successors(node):
                    if not graph.node[successor]['_tested']:
                        cancel = True
                if cancel: continue
                in_degree = graph.in_degree(node, weight='weight')
                #transfer your self coverage to yours parents
                for predecessor in graph.predecessors(node):
                    part = float(graph.edge[predecessor][node]['weight'])/in_degree
                    graph.node[predecessor]['_self_coverage'] = graph.node[predecessor]['_self_coverage'] + graph.node[node]['_self_coverage'] * part
                    #Maybe this node is not small anymore
                    if graph.node[predecessor]['_self_coverage'] >= 1 and graph.node[predecessor]['_small']:
                        graph.node[predecessor]['_small'] = False
                if graph.predecessors(node):
                    graph.node[node]['_self_coverage'] = 0
                graph.node[node]['_tested'] = True

        with open("{0}/loops".format(cere_configure.default_measures_path), 'w') as f:
            newLoopsToTest = False
            for n, d in graph.nodes(data=True):
                if d['_valid'] and not d['_small'] and not d['_tested']:
                    newLoopsToTest = True
                    f.write("__invivo__"+suppr_prefix(d['_name'])+"\n")

        plot(graph, step)
        save_graph(graph)

        if not newLoopsToTest: break
        else:
            os.system("~/loop_extractor/src/granularity/matching.sh . {0}/loops {1} {2}".format(cere_configure.default_measures_path, binary_cmd, compile_cmd))

    return True
