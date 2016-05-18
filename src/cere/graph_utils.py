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

import networkx as nx
from networkx.drawing.nx_agraph import write_dot
import cPickle as pickle
import logging
import os
import subprocess
import vars as var

logger = logging.getLogger('Graph utils')

def plot(g, step=""):
    import os
    for n,d in g.nodes(data=True):
        d["label"]="{} {} {} ({})".format(n, d['_name'], d['_self_coverage'], d['_coverage'])
        if not d['_valid'] or not d['_tested']: d["style"]="dotted"
        else: d["style"]="solid"
        if d['_tested']:
            d["style"]="solid"
            if not d['_matching']: d['color']="red"
            else: d['color']="green"
        if d['_to_test']: d['color']="orange"
    for u,v,d in g.edges(data=True):
        d["label"] = round(d["weight"], 2)
    write_dot(g,"{0}/graph_{1}.dot".format(var.CERE_PROFILE_PATH, step))
    try:
        logger.debug(subprocess.check_output("dot -Tpdf {0}/graph_{1}.dot -o {0}/graph_{1}.pdf".format(var.CERE_PROFILE_PATH, step), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.error("Cannot create the pdf")

def load_graph(step=""):
    if not os.path.isfile("{0}/graph_{1}.pkl".format(var.CERE_PROFILE_PATH, step)): return None
    with open("{0}/graph_{1}.pkl".format(var.CERE_PROFILE_PATH, step), 'rb') as input:
        graph = pickle.load(input)
    return graph

def save_graph(g, step=""):
    with open("{0}/graph_{1}.pkl".format(var.CERE_PROFILE_PATH, step), 'wb') as output:
        pickle.dump(g, output, pickle.HIGHEST_PROTOCOL)
