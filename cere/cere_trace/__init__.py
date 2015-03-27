#!/usr/bin/env python

from __future__ import print_function
import os
import sys
import shutil
import argparse
import logging
import subprocess
import cere_configure
import cere_instrument
import networkx as nx
from common.graph_utils import load_graph
import common.variables as var
import common.utils as utils

def init_module(subparsers, cere_plugins):
    cere_plugins["trace"] = run
    trace_parser = subparsers.add_parser("trace", help="trace a region")
    trace_parser.add_argument('--region', help="Region to trace")
    trace_parser.add_argument('--regions-file', help="File containing the list of regions to trace")
    trace_parser.add_argument('--norun', type=bool, const=True, default=False, nargs='?', help="=If you don't want to automatically run the trace")
    trace_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-trace any previous CERE trace")


def get_region_id(region, graph):
    for n, d in graph.nodes(data=True):
        if d['_name'] == region: return n
    return None

def trace_exists(region):
    if os.path.isfile("{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], region))\
    and os.path.isfile("{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], region)):
        return True
    else: return False

def comment_traced_region(regions_file):
    loops_to_trace = []
    with open(regions_file, 'r+') as f:
        regions = f.readlines()
        f.seek(0)
        f.truncate()
        for region in regions:
            if trace_exists(region.rstrip().replace('#', '')) or utils.is_invalid(region):
                if utils.is_invalid(region):
                    logging.error("Region {0} is invalid".format(region.rstrip().replace('#', '')))
                else:
                    logging.info("Trace already exists for region {0}".format(region.rstrip().replace('#', '')))
                if region.startswith('#'): f.write(region)
                else: f.write("#" + region)
            else:
                if region.startswith('#'): f.write(region.replace('#', ''))
                else: f.write(region)
                loops_to_trace.append(region.rstrip().replace('#', ''))
    return loops_to_trace

def find_region_to_trace(args, graph, trace_file):
    loops_to_trace = []
    region_node = get_region_id(args.region, graph)
    f = open(trace_file, "w")
    if not region_node:
        logging.error("Can't measure multiple trace. Region {0} not found in the call graph".format(args.region))
        return False

    #Find roots
    roots = [n for n,d in graph.in_degree().items() if d==0]

    #Compute for every nodes, the max distance from himself to each root
    max_path_len = {}
    for root in roots:
        for n, d in graph.nodes(data=True):
            if n not in max_path_len: max_path_len[n]=0
            #Get every paths from the current root to the node
            paths = list(nx.all_simple_paths(graph, root, n))
            #Keep the max length path
            for path in paths:
                if len(path)-1 > max_path_len[n]:
                    max_path_len[n] = len(path)-1
    #Keep region which have the same depth than the requested region
    for n, p in max_path_len.iteritems():
        if p == max_path_len[region_node]:
            reg = graph.node[n]['_name']
            if (not trace_exists(reg) and not utils.is_invalid(reg)) or args.force:
                print(reg, file=f)
                loops_to_trace.append(reg)
                logging.info("Region {0} added to regions to trace together".format(reg))
            elif utils.is_invalid(reg):
                logging.error("Region {0} is invalid".format(reg))
            else:
                logging.info("Trace already exists for region {0}".format(reg))
    f.close()
    return loops_to_trace

def run(args):
    if not (args.region or args.regions_file):
        logging.critical("No action requested, add --region or --regions_file")
        return False
    cere_configure.init()
    loops_to_trace = []
    args.trace_file = None
    #If we want to trace a list of regions
    if args.regions_file:
        if not os.path.isfile(args.regions_file):
            logging.critical("No such file: {0}".format(args.regions_file))
            return False
        args.regions_file = os.path.abspath(args.regions_file)
        #Comment region if we already measured its trace
        if not args.force:
            loops_to_trace = comment_traced_region(args.regions_file)
        logging.info("Compiling trace mode for region(s) in file {0}".format(args.regions_file))
    #If we want to trace a single region and multiple trace activated
    elif cere_configure.cere_config["multiple_trace"]:
        logging.info("Multiple trace activate, looking for regions to trace together")
        #If the call graph is available, we can trace multiple regions at the same time
        #to improve the instrumentation time needed.
        graph = load_graph()
        if graph:
            trace_file = os.path.abspath("{0}/cere_loops_to_trace".format(cere_configure.cere_config["cere_measures_path"]))
            loops_to_trace = find_region_to_trace(args, graph, trace_file)
            args.trace_file = trace_file
            logging.info("Tracing for region(s) in file {0}".format(trace_file))
        #No graph, we just trace the requested loop if the trace does not already exists
        else:
            logging.error("Can't trace multiple region: Region call graph not available.\n\
                          Run cere profile.\n\
                          Tracing region {0}".format(args.region))
            if (not trace_exists(args.region) and not utils.is_invalid(args.region)) or args.force:
                loops_to_trace.append(args.region)
            elif utils.is_invalid(args.region):
                logging.error("Region {0} is invalid".format(args.region))
            else:
                logging.info("Trace already exists for region {0}".format(args.region))
                return True
    #If we want to trace a single region multiple trace not activated
    elif (not trace_exists(args.region) and not utils.is_invalid(args.region)) or args.force:
        loops_to_trace.append(args.region)
        logging.info("Tracing region {0}".format(args.region))
    elif utils.is_invalid(args.region):
        logging.error("{0} is invalid".format(args.region))
        return False
    else:
        logging.info("Trace already exists for region {0}".format(args.region))
        return True

    if len(loops_to_trace) == 0:
        logging.info("No loops to trace")
        return True
    args.invocation=0
    args.wrapper = var.RDTSC_WRAPPER
    os.environ["CERE_TRACE"] = "1"
    cere_instrument.run(args)

    err = False
    for region in loops_to_trace:
        try:
            shutil.move("{0}.bin".format(region), "{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], region))
            shutil.move("{0}.csv".format(region), "{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], region))
        except IOError as err:
            logging.critical(str(err))
            logging.critical("Trace failed for region {0}: No output files, maybe the selected region does not exist.".format(region))
            utils.mark_invalid(region)
            err = True
    del os.environ["CERE_TRACE"]
    return not err
