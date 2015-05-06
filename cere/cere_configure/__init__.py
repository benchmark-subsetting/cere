#!/usr/bin/env python

import os
import shutil
import sys
import json
import argparse
import logging
import common.variables as var

cere_config={}
logger = logging.getLogger('Configure')

def init_module(subparsers, cere_plugins):
    cere_plugins["configure"] = run
    configure_parser = subparsers.add_parser("configure", help="Configure CERE to build and run an application")
    configure_parser.add_argument('--build_cmd', required=True, help="Sets the command to build the application")
    configure_parser.add_argument('--run_cmd', required=True, help="Sets the command used to run the application")
    configure_parser.add_argument('--multiple_trace', action='store_true', help="Enables tracing multiple regions in a single run. (default false)")

def run(args):
    global cere_config
    cere_config["build_cmd"] = args.build_cmd
    cere_config["run_cmd"] = args.run_cmd
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
    if not setup_dir():
        logger.critical("Cannot create required directories for CERE. Check permissions?")
        return False
    return True

def setup_dir():
    for path in var.CERE_DIRECTORIES:
        if not os.path.isdir(path):
            try:
                os.makedirs(path)
            except OSError as err:
                logger.error(str(err))
                return False
    return True
