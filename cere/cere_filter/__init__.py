#!/usr/bin/env python

import argparse
import logging
import cere_configure
import os
from create_graph import create_graph
import cere_test
from update_graph import update

def init_module(subparsers, cere_plugins):
    cere_plugins["filter"] = run
    profile_parser = subparsers.add_parser("filter", help="Filter regions")
    profile_parser.add_argument("--max_error", default=15.0, help="Maximum tolerated error between invivo and invitro regions")
    profile_parser.add_argument("--min_coverage", type=float, default=0.1, help="Minimum percentage of region execution time")
    profile_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will overwrite any previous CERE measures")

def check_arguments(args):
    return True

def check_dependancies(args):
    #Check if the profiling file is present
    profile_file = "{0}/app.prof".format(cere_configure.cere_config["cere_measures_path"])
    if not os.path.isfile(profile_file):
        logging.critical('No profiling file')
        logging.info('Run: cere profile --regions')
        return False
    return True

def run(args):
    cere_configure.init()
    if not check_dependancies(args):
        return False
    if not check_arguments(args):
        return False
    if not create_graph(args.min_coverage/100, args.force):
        return False
    #Find matching codelets
    if not update(args):
        return False
    #Select matching codelets with best coverage
    from ilp_selector import *
    if not solve_with_best_granularity(args.max_error):
        return False
    return True
