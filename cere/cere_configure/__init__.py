#!/usr/bin/env python

import os
import shutil
import sys
import json
import argparse
import logging

cere_config={}
logger = logging.getLogger('Configure')

def init_module(subparsers, cere_plugins):
    cere_plugins["configure"] = run
    configure_parser = subparsers.add_parser("configure", help="Configure CERE to work with your application")
    configure_parser.add_argument('--run_cmd', required=True, help="Set the command to run your application")
    configure_parser.add_argument('--build_cmd', required=True, help="Set the command to build the application")
    configure_parser.add_argument('--measures_path', default="cere_measures", help="Directory where to save CERE measures (Default: cere_measures)")
    configure_parser.add_argument('--dumps_path', default="cere_dumps", help="Directory where to save CERE regions dumps (Default: cere_dumps)")
    configure_parser.add_argument('--multiple_trace', const=True, default=False, nargs='?', help="Enable tracing multiple region at the same time (Default: False)")

def run(args):
    global cere_config
    cere_config["build_cmd"] = args.build_cmd
    cere_config["run_cmd"] = args.run_cmd
    cere_config["cere_measures_path"] = args.measures_path
    cere_config["cere_dumps_path"] = args.dumps_path
    cere_config["multiple_trace"] = args.multiple_trace

    with open("cere.json", 'w') as config_file:
        json.dump(cere_config, config_file)
    logger.info("Configuration success")
    return True

def init():
    global cere_config
    if not os.path.isfile("cere.json"):
        logger.critical("No configuration file found. Run: cere configure")
        return False
    with open("cere.json", 'r') as config_file:
        cere_config = json.load(config_file)
    if not setup_dir(cere_config["cere_measures_path"]):
        logger.critical("Cannot create required directories for CERE. Check permissions?")
        return False
    return True

def setup_dir(measures_path):
    if not os.path.isdir(measures_path):
        try:
            os.mkdir(measures_path)
        except OSError as err:
            logger.error(str(err))
            return False
    if not os.path.isdir("{0}/plots".format(measures_path)):
        try:
            os.mkdir("{0}/plots".format(measures_path))
        except OSError as err:
            logger.error(str(err))
            return False
    return True
