#!/usr/bin/env python

import argparse
import logging
import cere_configure
import os
from create_graph import create_graph
#~ from update_graph import update
#~ from get_app_cycles import *
#~ from granularity import *

def init_module(subparsers, cere_plugins):
    cere_plugins["filter"] = run
    profile_parser = subparsers.add_parser("filter", help="Filter regions")
    profile_parser.add_argument("--max_error", default=15.0, help="Maximum tolerated error between invivo and invitro regions")
    profile_parser.add_argument("--min_coverage", default=0.001, help="Minimum percentage of region execution time")
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
    if not create_graph(cere_configure.cere_config["run_cmd"], cere_configure.cere_config["build_cmd"], args.min_coverage, args.force):
        return False
    
    #~ #Find matching codelets
    #~ if not update(binary_cmd, compile_cmd, error):
        #~ return False
    #~ #Select matching codelets with best coverage
    #~ if not solve_with_best_granularity(error):
        #~ return False
    return True
