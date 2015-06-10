#!/usr/bin/env python
# This file is part of CERE.
#
# Copyright (c) 2013-2015, Universite de Versailles St-Quentin-en-Yvelines
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
import csv
import cPickle as pickle
import networkx as nx
import subprocess
from common.graph_utils import *
import cere_configure
import cere_check_matching
import logging
import common.variables as var
import common.utils as utils

logger = logging.getLogger('ILP selector')

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
                if utils.is_invalid(d['_name']):
                    d['_error_message'] = utils.get_error_message(d['_name'])
                invocations = read_csv("{0}/invocations_error.csv".format(var.CERE_REPLAY_PATH))
                for inv in invocations:
                    if inv["Codelet Name"] == d['_name']:
                        d['_invocations'].append({"Cluster":inv["Cluster"], "Invocation":inv["Invocation"],
                          "Part":inv["Part"], "Invivo (cycles)":"{:e}".format(float(inv["Invivo"])),
                          "Invitro (cycles)":"{:e}".format(float(inv["Invitro"])), "Error (%)":float(inv["Error"])})
                d['_tested'] = True
                d['_to_test'] = False
    return graph

def update(args):
    binary_cmd = cere_configure.cere_config["run_cmd"]
    build_cmd = cere_configure.cere_config["build_cmd"]
    error = args.max_error
    args.regions_file = "{0}/loops".format(var.CERE_REPLAY_PATH)

    logger.info("Start graph updating")
    graph = load_graph("original")
    if graph == None:
        logger.critical("No graph to load. Did you run cere profile?")
        return False

    step=0
    while(1):
        step = step + 1
        if step != 1:
            #1) Something new?
            lines = read_csv("{0}/matching_error.csv".format(var.CERE_REPLAY_PATH))
            graph = update_nodes(graph, lines, error)

        newLoopsToTest = False
        with open(args.regions, 'w') as f:
            for n, d in graph.nodes(data=True):
                cancel=False
                if d['_coverage'] < 1: d['_small'] = True
                if d['_valid'] and not d['_small'] and not d['_tested']:
                    newLoopsToTest = True
                    d['_to_test']=True
                    f.write(d['_name']+"\n")

        if not newLoopsToTest:
            plot(graph, "final")
            save_graph(graph)
            break
        else:
            plot(graph, step)
            save_graph(graph)
            cere_check_matching.run(args)

    return True
