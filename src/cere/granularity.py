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
import vars as var
import logging
import csv
import utils
from pulp import LpInteger, LpMinimize, LpProblem, LpStatus, LpVariable, lpSum, GLPK

logger = logging.getLogger('ILP selector')
tolerated_error = [5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,99.9]

class Unsolvable(Exception):
    pass

def solve(graph, err, max_coverage=100, step=5):
    coverage = max_coverage
    for n,d in graph.nodes(data=True):
        d['_matching'] = True
        d["_selected"] = False
        if d['_error'] > err: d['_matching'] = False
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

    # Objective function: minimize the total replay cost of selected codelets

    # Compute replay time
    for n,d in graph.nodes(data=True):
      d['_total_replay_cycles'] = 0
      for inv in d['_invocations']:
        d['_total_replay_cycles'] = d['_total_replay_cycles'] + float(inv["Invivo (cycles)"])

    prob += lpSum([codelet_vars[n]*d['_total_replay_cycles'] for n,d in graph.nodes(data=True)])

    # and with good coverage
    prob += (lpSum([codelet_vars[n]*d['_coverage'] for n,d in graph.nodes(data=True)]) >= min_coverage)

    # selected codelets should match
    for n,d in graph.nodes(data=True):
        if not d['_matching']:
            prob += codelet_vars[n] == 0

    # Finally we should never include both the children and the parents
    for dad in graph.nodes():
        for son in graph.nodes():
            if not dad in nx.ancestors(graph, son):
                continue
            # We cannot select dad and son at the same time
            prob += codelet_vars[dad] + codelet_vars[son] <= 1

    #prob.solve(GLPK())
    prob.solve()
    if (LpStatus[prob.status] != 'Optimal'):
        raise Unsolvable()

    for v in prob.variables():
        assert v.varValue == 1.0 or v.varValue == 0.0
        if v.varValue == 1.0:

            for n,d in graph.nodes(data=True):
                if ("codelet_"+str(n)) == v.name:
                    d["_selected"] = True
                    yield n

def solve_with_best_granularity(error):
    target_error = error
    assert(target_error in tolerated_error)

    graph = load_graph()
    if graph == None:
        logger.critical("Cannot load graph. Did you run cere profile?")
        return False

    if( len(graph.nodes()) == 0):
        logger.info('Graph is empty, nothing to select')
        return True
    error_filename = "{0}/table_error.csv".format(var.CERE_REPORT_PATH)
    padding = max([len(d['_name']) for n,d in graph.nodes(data=True)])

    table = utils.Error_table()
    target_error_chosen = set()
    graph.graph['coverage'] = 0

    for err in tolerated_error:
        logger.info("Computing matching with a maximum error of {0}%".format(err))
        try:
            chosen, coverage = solve(graph, err)
            table.complete_error_table(err, coverage)
        except(Unsolvable):
            coverage=0
            table.complete_error_table(err, coverage)

    try:
        target_error_chosen, target_coverage = solve(graph, target_error)
    except(Unsolvable):
        target_coverage=0
        logger.error("Solution impossible")

    
    table.write_table(error_filename)
    logger.info("Solved with coverage >= {0}".format(target_coverage))
    for c in target_error_chosen:
        graph.graph['coverage'] = graph.graph['coverage'] + graph.node[c]['_coverage']
        print >>sys.stderr, "> {0} {1}".format(graph.node[c]['_name'].ljust(padding), graph.node[c]['_coverage'])
    graph.graph['selector'] = "ILP"
    save_graph(graph)
    return True
