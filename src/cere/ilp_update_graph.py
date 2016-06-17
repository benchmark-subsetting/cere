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
import subprocess
from graph_utils import *
import cere_configure
import cere_check_matching
import logging
import vars as var
import utils

logger = logging.getLogger('ILP selector')

def update(args):
    binary_cmd = cere_configure.cere_config["run_cmd"]
    build_cmd = cere_configure.cere_config["build_cmd"]
    args.regions_file = "{0}/loops".format(var.CERE_REPLAY_PATH)
    args.region = None

    logger.info("Start graph updating")
    graph = load_graph()
    if graph == None:
        logger.critical("No graph to load. Did you run cere profile?")
        return False

    while(1):
        graph = load_graph()

        newLoopsToTest = False
        with open(args.regions_file, 'w') as f:
            for n, d in graph.nodes(data=True):
                if d['_coverage'] < args.min_coverage: d['_small'] = True
                if d['_valid'] and not d['_small'] and not d['_tested']:
                    newLoopsToTest = True
                    d['_to_test']=True
                    f.write(d['_name']+"\n")

        plot(graph)
        save_graph(graph)
        if not newLoopsToTest:
            break
        else:
            cere_check_matching.run(args)

    return True
