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

import os
import re
import subprocess
import cere_configure
import logging
import networkx as nx
import vars as var
from graph_utils import plot, save_graph

logger = logging.getLogger('Profile')

def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath = program.split()
    for v in fpath:
        if is_exe(v):
            return v
    return None

def parse_line(regex_list, line):
    i=-1
    matchObj=""
    while not matchObj:
        try:
            i = i + 1
            matchObj = re.match( regex_list[i], line )
        except IndexError:
            break
    return matchObj, i

def delete_useless_nodes(graph):
    parents=[]
    childs=[]
    nodes = (list(reversed(nx.topological_sort(graph))))
    step=0
    for n in nodes:
        #We have to remove this node
        if not graph.node[n]['_valid']:
            in_degree = graph.in_degree(n, weight='weight')
            for predecessor in graph.predecessors(n):
                part = round(float(graph.edge[predecessor][n]['weight'])/in_degree, 2)
                graph.node[predecessor]['_self_coverage'] = round(graph.node[predecessor]['_self_coverage'] + graph.node[n]['_self_coverage'] * part, 2)
                for successor in graph.successors(n):
                    graph.add_edge(predecessor, successor, weight=round(graph.edge[predecessor][n]['weight']*(float(graph.edge[n][successor]['weight'])/in_degree), 2))
            graph.remove_node(n)
    return True

#Fix the self coverage for leaves
def fix_self_coverage(graph, samples):
    nodes = (list(reversed(nx.topological_sort(graph))))
    for n in nodes:
        in_degree = graph.in_degree(n, weight='weight')
        out_degree = graph.out_degree(n, weight='weight')
        #Don't touch root because we don't know the real in_degree
        if in_degree == 0: continue
        graph.node[n]['_self_coverage'] = round(((in_degree - out_degree)/float(samples))*100, 1)
    return True

def add_node(digraph, matchObj):
    _id = matchObj.group(1)
    name = matchObj.group(2)

    try:
        coverage = float(matchObj.group(6))
    except IndexError:
        coverage = float(matchObj.group(4))

    if "__cere__"  in name: valid = True
    else:
        valid = False

    digraph.add_node(_id, _name = name)
    digraph.node[_id]['_self_coverage'] = float(matchObj.group(4))
    digraph.node[_id]['_coverage'] = coverage
    digraph.node[_id]['_matching'] = False
    digraph.node[_id]['_error'] = 100.0
    digraph.node[_id]['_error_message'] = None
    digraph.node[_id]['_valid'] = valid
    digraph.node[_id]['_tested'] = False
    digraph.node[_id]['_to_test'] = False
    digraph.node[_id]['_transfered'] = False
    digraph.node[_id]['_small'] = False
    digraph.node[_id]['_selected'] = False
    digraph.node[_id]['_invivo'] = 0.0
    digraph.node[_id]['_invitro'] = 0.0
    digraph.node[_id]['_invocations'] = []
    return digraph

def remove_cycle(digraph, cycle, samples):
    #Avoid having the same node appears multiple times
    #i.e. For recursive calls
    cycle = list(set(cycle))
    parents = []
    childs = []
    toKeep = cycle[0]
    for node in cycle:
        #find parents
        for predecessor in digraph.predecessors(node):
            if predecessor not in cycle:
                parents.append({'id' : predecessor, 'weight' : digraph.edge[predecessor][node]['weight']})
        #find childs
        for successor in digraph.successors(node):
            if successor not in cycle:
                childs.append({'id' : successor, 'weight' : digraph.edge[node][successor]['weight']})
        #keep the node with the highest coverage
        if digraph.node[node]['_coverage'] > digraph.node[toKeep]['_coverage']:
            toKeep = node
    #Backup the node to keep
    replacer = digraph.node[toKeep]
    #remove the cycle
    digraph.remove_nodes_from(cycle)
    #replace it by the node to keep
    digraph.add_node(toKeep, replacer)
    #restore edges
    for parent in parents:
        if digraph.has_edge(parent['id'], toKeep):
            w = int(digraph.edge[parent['id']][toKeep]['weight'] + parent['weight'])
        else:
            w = int(parent['weight'])
        digraph.add_edge(parent['id'], toKeep, weight=w)
    for child in childs:
        if digraph.has_edge(toKeep, child['id']):
            w = int(digraph.edge[toKeep][child['id']]['weight'] + child['weight'])
        else:
            w = int( child['weight'])
        digraph.add_edge(toKeep, child['id'], weight=w)
    #Update the coverage of the new node. We don't need to do it
    #for self coverage as it will be updated by the fonction fix_self_coverage.
    in_degree = digraph.in_degree(toKeep, weight='weight')
    digraph.node[toKeep]['_coverage'] = round(((in_degree)/float(samples))*100, 1)
    return digraph

def remove_cycles(digraph, sample):
    cycles = list(nx.simple_cycles(digraph))
    while cycles:
        digraph = remove_cycle(digraph, cycles[0], sample)
        cycles = list(nx.simple_cycles(digraph))
    return digraph

def parse_gPerfTool(digraph, cmd, regex_list):
    samples=0
    for line in cmd.stdout:
        matchObj, step = parse_line(regex_list, line)
        if step < 2:
            digraph = add_node(digraph, matchObj)
        elif step == 2:
            digraph.add_edge(matchObj.group(1), matchObj.group(2), weight=int(matchObj.group(3)))
        elif step == 3:
            samples = int(matchObj.group(1))
        else:
            continue
    return samples, digraph

def create_graph(force):
    run_cmd = cere_configure.cere_config["run_cmd"]
    build_cmd = cere_configure.cere_config["build_cmd"]
    clean_cmd = cere_configure.cere_config["clean_cmd"]
    logger.info('Start call graph creation')

    #Build again the application to be sure we give the right binary to pprof
    try:
        logger.debug(subprocess.check_output("{0} && {1} CERE_MODE=\"original --instrument --instrument-app\"".format(clean_cmd, build_cmd), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        return False

    binary = which(run_cmd)
    if not binary:
        logger.critical("Cannot find the binary. Please provide binary name through cere configure --binary.")
        return False

    profile_file = "{0}/app.prof".format(var.CERE_PROFILE_PATH)
    if not os.path.isfile(profile_file):
        logger.critical("No profiling file. Please run cere profile.")
        return False

    #regular expression to parse the gperf tool output
    regex_list = [r'(N.*)\s\[label\=\"(.*?)\\n([0-9]*)\s\((.*)\%\)\\rof\s(.*)\s\((.*)\%\)\\r',
                  r'(N.*)\s\[label\=\"(.*)\\n([0-9]*)\s\((.*)\%\)\\r',
                  r'(N.*)\s\-\>\s(N.*)\s\[label\=([0-9]*)\,',
                  r'Legend\s\[.*Total samples:\s([0-9]*).*\]']

    cmd = subprocess.Popen("{0} -dot {1} {2}".format(var.PPROF, binary, profile_file), shell=True, stdout=subprocess.PIPE)

    digraph = nx.DiGraph()
    digraph.graph['coverage'] = 0
    digraph.graph['selector'] = None
    samples, digraph = parse_gPerfTool(digraph, cmd, regex_list)
    plot(digraph, "debug")
    digraph = remove_cycles(digraph, samples)
    if not fix_self_coverage(digraph, samples):
        return False
    if not delete_useless_nodes(digraph):
        return False

    plot(digraph)
    save_graph(digraph)

    logger.info('Call graph creation successful')
    return True
