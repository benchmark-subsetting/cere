#!/usr/bin/env python

import sys
import os
import cPickle as pickle
import networkx as nx
from common.graph_utils import *
import cere_configure
import update_graph
import logging
import csv
import common.utils as utils

logger = logging.getLogger('Max-Cov selector')

def solve(graph):
    coverage = 0
    nodes = []
    for n, d in graph.nodes(data=True):
        d["_selected"] = False
        if d['_matching']:
            coverage = coverage + d['_self_coverage']
            nodes.append(n)
            d["_selected"] = True
    return nodes, coverage

def solve_with_best_granularity(args):
    graph = load_graph()
    if graph == None:
        logger.critical("Can't load graph. did you run cere profile?")
        return False

    if( len(graph.nodes()) == 0):
        logger.info('Graph is empty, nothing to select')
        return True

    #Compute coverage for different error
    error_filename = "{0}/table_error.csv".format(cere_configure.cere_config["cere_measures_path"])
    table = utils.Error_table()
    args.max_error = 100
    while args.max_error >= 5:
        logger.info("Computing matching with a maximum error of {0}%".format(args.max_error))
        update_graph.update(args)
        graph = load_graph()
        table_chosen, table_coverage = solve(graph)
        table.complete_error_table(args.max_error, table_coverage)
        args.max_error = args.max_error-5
    table.write_table(error_filename)

    args.max_error = 15
    update_graph.update(args)
    graph = load_graph()
    padding = max([len(d['_name']) for n,d in graph.nodes(data=True)])
    chosen, coverage = solve(graph)

    if coverage == 0:
        logger.error("Solution impossible")
    else:
        logger.info("Solved with coverage >= {0}".format(coverage))

    graph.graph['coverage'] = 0
    for c in chosen:
        graph.graph['coverage'] = graph.graph['coverage'] + graph.node[c]['_self_coverage']
        print >>sys.stderr, "> {0} {1}".format(graph.node[c]['_name'].ljust(padding), graph.node[c]['_self_coverage'])
    save_graph(graph)
    return True
