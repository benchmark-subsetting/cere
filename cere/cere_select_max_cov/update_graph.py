#!/usr/bin/env python

import sys
import os
import csv
import cPickle as pickle
import networkx as nx
import subprocess
from common.graph_utils import *
import cere_configure
import cere_test
import logging

def read_csv(File):
    try:
        FILE = open(File, 'rb')
    except (IOError):
        return []
    Dict = csv.DictReader(FILE, delimiter=',')
    return Dict

def update_nodes(graph, lines, max_allowed_error):
    for line in lines:
        #for region_name, error in matching.iteritems():
        #find the node in the graph
        for n,d in graph.nodes(data=True):
            if line["Codelet Name"] == d['_name'] and not d['_tested']:
                d['_invivo'] = float(line["Invivo"])
                d['_invitro'] = float(line["Invitro"])
                d['_tested'] = True
                if float(line["Error"]) <= max_allowed_error:
                    d['_matching'] = True
                else:
                    d['_valid'] = False
                d['_error'] = float(line["Error"])
                invocations = read_csv("{0}/invocations_error.csv".format(cere_configure.cere_config["cere_measures_path"]))
                for inv in invocations:
                    if inv["Codelet Name"] == d['_name']:
                        d['_invocations'].append({"Cluster":inv["Cluster"], "Invocation":inv["Invocation"],
                          "Part":round(float(inv["Part"]), 2), "Invivo (cycles)":"{:e}".format(float(inv["Invivo"])),
                          "Invitro (cycles)":"{:e}".format(float(inv["Invitro"])), "Error (%)":round(float(inv["Error"]), 2)})
                d['_tested'] = True
                d['_to_test'] = False
    return graph

def update(args):
    binary_cmd = cere_configure.cere_config["run_cmd"]
    build_cmd = cere_configure.cere_config["build_cmd"]
    error = args.max_error
    args.regions = "{0}/loops".format(cere_configure.cere_config["cere_measures_path"])
    args.force = False

    logging.info("Start graph updating")
    graph = load_graph("original")
    if graph == None:
        logging.critical("No graph to load")
        return False

    step=0
    while(1):
        step = step + 1
        if step != 1:
            #1) Something new?
            lines = read_csv("{0}/matching_error.csv".format(cere_configure.cere_config["cere_measures_path"]))
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
                #if all my parent's sons are not matching, transfert my coverage
                if graph.node[node]['_transfered']: continue
                for predecessor in graph.predecessors(node):
                    part = round(float(graph.edge[predecessor][node]['weight'])/in_degree, 2)
                    graph.node[predecessor]['_self_coverage'] = round(graph.node[predecessor]['_self_coverage'] + graph.node[node]['_self_coverage'] * part, 2)
                    #Maybe this node is not small anymore
                    if graph.node[predecessor]['_self_coverage'] >= 1 and graph.node[predecessor]['_small']:
                        graph.node[predecessor]['_small'] = False
                graph.node[node]['_transfered'] = True

        newLoopsToTest = False
        with open(args.regions, 'w') as f:
            for n, d in graph.nodes(data=True):
                cancel=False
                if d['_valid'] and not d['_small'] and not d['_tested']:
                    for successor in graph.successors(n):
                        #if a successor is not tested yet, we don't test this region
                        if not graph.node[successor]['_tested'] and not graph.node[successor]['_small']:
                            cancel = True
                    if cancel: continue
                    newLoopsToTest = True
                    d['_to_test']=True
                    f.write(d['_name']+"\n")

        save_graph(graph)
        if not newLoopsToTest:
            plot(graph, "final")
            break
        else:
            plot(graph, step)
            cere_test.run(args)

    return True
