#!/usr/bin/env python

from __future__ import print_function
import os
import sys
import shutil
import argparse
import logging
import subprocess
import cere_configure
import networkx as nx
from common.graph_utils import load_graph
import common.variables as var

def get_region_id(region, graph):
    for n, d in graph.nodes(data=True):
        if d['_name'].replace("extracted", "invivo") == region: return n
    return None

def trace_exists(region):
    if os.path.isfile("{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], region))\
    and os.path.isfile("{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], region)):
        return True
    else: return False

def init_module(subparsers, cere_plugins):
    cere_plugins["trace"] = run
    trace_parser = subparsers.add_parser("trace", help="trace a region")
    trace_parser.add_argument('--region', help="Region to trace")
    trace_parser.add_argument('--regions-file', help="File containing the list of regions to trace")
    trace_parser.add_argument('--norun', type=bool, const=True, default=False, nargs='?', help="=If you don't want to automatically run the trace")
    trace_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-trace any previous CERE trace")

def run(args):
    if not (args.region or args.regions_file):
        logging.critical("No action requested, add --region or --regions_file")
        return False
    cere_configure.init()
    region_input = ""
    loops_to_trace= []
    #If we want to trace a list of regions
    if args.regions_file:
        #Comment region if we already measured its trace
        if not args.force:
            with open(args.regions_file, 'r+') as f:
                regions = f.readlines()
                f.seek(0)
                f.truncate()
                for region in regions:
                    if trace_exists(region.rstrip().replace('#', '')):
                        logging.info("Trace already exists for region {0}".format(region.rstrip().replace('#', '')))
                        if region.startswith('#'): f.write(region)
                        else: f.write("#" + region)
                    else:
                        if region.startswith('#'): f.write(region.replace('#', ''))
                        else: f.write(region)
                        loops_to_trace.append(region.rstrip().replace('#', ''))
        region_input = "--regions-file={0}".format(args.regions_file)
        logging.info("Compiling trace mode for region(s) in file {0}".format(args.regions_file))
    #If we want to trace a single region
    elif cere_configure.cere_config["multiple_trace"]:
        #If the call graph is available, we can trace multiple regions at the same time
        #to improve the instrumentation time needed.
        graph = load_graph()
        if graph:
            region_node = get_region_id(args.region, graph)
            trace_file = os.path.abspath("{0}/cere_loops_to_trace".format(cere_configure.cere_config["cere_measures_path"]))
            print(trace_file)
            f = open(trace_file, "w")
            if not region_node:
                logging.error("Can't measure multiple trace. Region {0} not found in the call graph".format(args.region))
                return False
            if not trace_exists(args.region) or args.force:
                print(args.region, file=f)
                loops_to_trace.append(args.region)
            for n, d in graph.nodes(data=True):
                if not nx.has_path(graph, n, region_node) and not nx.has_path(graph, region_node, n):
                    if not trace_exists(d['_name'].replace("extracted", "invivo")) or args.force:
                        print(d['_name'].replace("extracted", "invivo"), file=f)
                        loops_to_trace.append(d['_name'].replace("extracted", "invivo"))
            f.close()
            region_input = "--regions-file={0}".format(trace_file)
            logging.info("Compiling trace mode for region(s) in file {0}".format(trace_file))
        elif not trace_exists(args.region) or args.force:
            region_input = "--region={0}".format(args.region)
            loops_to_trace.append(args.region)
            logging.info("Compiling trace mode for region {0}".format(args.region))
        else:
            logging.info("Trace already exists for region {0}".format(args.region))
            return True
    elif not trace_exists(args.region) or args.force:
        region_input = "--region={0}".format(args.region)
        loops_to_trace.append(args.region)
        logging.info("Compiling trace mode for region {0}".format(args.region))
    else:
        logging.info("Trace already exists for region {0}".format(args.region))
        return True
    if len(loops_to_trace) == 0:
        logging.info("No loops to trace")
        return True
    err = False
    try:
        logging.debug(subprocess.check_output("{0} MODE=\"original {1} --instrument --trace --lib={2} --wrapper={3}\" -B".format(cere_configure.cere_config["build_cmd"], region_input, var.RDTSC_LIB, var.RDTSC_WRAPPER), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
        logging.info("Compiling trace mode for {0} failed".format(region_input))
        return False
    if not args.norun:
        logging.info("Tracing region {0}".format(region_input))
        try:
            logging.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.critical(str(err))
            logging.critical(err.output)
            logging.critical("Trace failed for {0}".format(region_input))
            return False
        for region in loops_to_trace:
            try:
                shutil.move("{0}.bin".format(region), "{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], region))
                shutil.move("{0}.csv".format(region), "{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], region))
            except IOError as err:
                logging.critical(str(err))
                logging.critical("Trace failed for region {0}: No output files, maybe the selected region does not exist.".format(region))
                err = True
    return not err
