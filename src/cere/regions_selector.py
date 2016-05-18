#!/usr/bin/env python
# This file is part of CERE.
#
# Copyright (c) 2013-2016, Universite de Versailles St-Quentin-en-Yvelines
#
# CERE is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# CERE is distributed in the hope that it will be useful,  
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with CERE.  If not, see <http://www.gnu.org/licenses/>.  

import sys
import os
import cPickle as pickle
import networkx as nx
from graph_utils import *
import max_cov_update_graph as update_graph
import logging
import csv
import utils
import vars as var

logger = logging.getLogger('Max-Cov selector')

def solve(graph, max_error):
    coverage = 0
    nodes = []
    for n, d in graph.nodes(data=True):
        d["_selected"] = False
        if d['_error'] <= max_error:
            coverage = coverage + d['_self_coverage']
            nodes.append(n)
            d["_selected"] = True
    if coverage > 100:
      coverage = 100
    return nodes, coverage

def solve_with_best_granularity(args):
    graph = load_graph()
    if graph == None:
        logger.critical("Can't load graph. did you run cere profile?")
        return False

    if( len(graph.nodes()) == 0):
        logger.info('Graph is empty, nothing to select')
        return True

    args.force=False
    #Compute coverage for different error
    error_filename = "{0}/table_error.csv".format(var.CERE_REPORT_PATH)
    table = utils.Error_table()
    args.max_error = 100
    while args.max_error >= 5:
        logger.info("Computing matching with a maximum error of {0}%".format(args.max_error))
        update_graph.update(args)
        graph = load_graph()
        table_chosen, table_coverage = solve(graph, args.max_error)
        table.complete_error_table(args.max_error, table_coverage)
        args.max_error = args.max_error-5
    table.write_table(error_filename)

    args.max_error = 15
    update_graph.update(args)
    graph = load_graph()
    padding = max([len(d['_name']) for n,d in graph.nodes(data=True)])
    chosen, coverage = solve(graph, args.max_error)

    if coverage == 0:
        logger.error("Solution impossible")
    else:
        logger.info("Solved with coverage >= {0}".format(coverage))

    graph.graph['coverage'] = 0
    for c in chosen:
        graph.graph['coverage'] = graph.graph['coverage'] + graph.node[c]['_self_coverage']
        print >>sys.stderr, "> {0} {1}".format(graph.node[c]['_name'].ljust(padding), graph.node[c]['_self_coverage'])
    graph.graph['selector'] = "MAX_COV"
    save_graph(graph)
    return True
