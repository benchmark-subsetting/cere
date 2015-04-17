#!/usr/bin/env python

import argparse
import logging
import cere_configure
import os
import cere_test
from update_graph import update
from granularity import *

logger = logging.getLogger('ILP selector')

def init_module(subparsers, cere_plugins):
    cere_plugins["select-ilp"] = run
    profile_parser = subparsers.add_parser("select-ilp", help="Select matching regions")
    profile_parser.add_argument("--max_error", default=15, type=int, help="Maximum tolerated error between invivo and invitro regions")
    profile_parser.add_argument("--min_coverage", type=float, default=0.1, help="Minimum percentage of region execution time")
    profile_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will overwrite any previous CERE measures")

def check_arguments(args):
    return True

def check_dependancies(args):
    #Check if the profiling file is present
    profile_file = "{0}/app.prof".format(cere_configure.cere_config["cere_measures_path"])
    graph = "{0}/graph_original.pkl".format(cere_configure.cere_config["cere_measures_path"])
    if not os.path.isfile(profile_file) or not os.path.isfile(graph):
        logger.critical('No profiling file or graph not created. Run: cere profile')
        return False
    return True

def run(args):
    if not cere_configure.init():
        return False
    if not check_dependancies(args):
        return False
    if not check_arguments(args):
        return False
    #Find matching codelets
    if not update(args):
        return False
    #Select matching codelets with best coverage
    if not solve_with_best_granularity(args.max_error):
        return False
    return True
