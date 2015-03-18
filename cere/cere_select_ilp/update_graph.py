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

LIST_PREFIX = ["__invivo__","__extracted__"]

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
            if suppr_prefix(line["Codelet Name"]) in d['_name'] and not d['_tested']:
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
                    if suppr_prefix(inv["Codelet Name"]) in d['_name']:
                        d['_invocations'].append({"Cluster":inv["Cluster"], "Invocation":inv["Invocation"],
                          "Part":inv["Part"], "Invivo (cycles)":"{:e}".format(float(inv["Invivo"])),
                          "Invitro (cycles)":"{:e}".format(float(inv["Invitro"])), "Error (%)":float(inv["Error"])})
                d['_tested'] = True
                d['_to_test'] = False
    return graph

def suppr_prefix(name):
    '''
    Remove prefix of a region
    '''
    for pre in LIST_PREFIX:
        name = name.replace(pre,"")
    return name

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

        newLoopsToTest = False
        with open(args.regions, 'w') as f:
            for n, d in graph.nodes(data=True):
                cancel=False
                if d['_valid'] and d['_coverage'] >= 1 and not d['_tested']:
                    newLoopsToTest = True
                    d['_to_test']=True
                    f.write("__invivo__"+suppr_prefix(d['_name'])+"\n")

        if not newLoopsToTest:
            plot(graph, "final")
            save_graph(graph)
            break
        else:
            plot(graph, step)
            save_graph(graph)
            cere_test.run(args)

    return True