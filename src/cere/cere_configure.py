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
import shutil
import sys
import json
import argparse
import logging
import vars as var

cere_config={}
logger = logging.getLogger('Configure')

def init_module(subparsers, cere_plugins):
    cere_plugins["configure"] = run
    configure_parser = subparsers.add_parser("configure", help="Configure CERE to build and run an application")
    configure_parser.add_argument('--build-cmd', required=True, help="Sets the command to build the application")
    configure_parser.add_argument('--run-cmd', required=True, help="Sets the command used to run the application")
    configure_parser.add_argument('--clean-cmd', required=True, help="Sets the command used to run the application")
    configure_parser.add_argument('--multiple-trace', action='store_true', help="Enables tracing multiple regions in a single run. (default false)")
    configure_parser.add_argument('--regions-infos', default="regions.csv", help="File in which regions infos are stored")

def run(args):
    global cere_config
    cere_config["build_cmd"] = args.build_cmd
    cere_config["run_cmd"] = args.run_cmd
    cere_config["clean_cmd"] = args.clean_cmd
    cere_config["multiple_trace"] = args.multiple_trace
    cere_config["regions_infos"] = args.regions_infos

    with open("cere.json", 'w') as config_file:
        json.dump(cere_config, config_file)
    logger.info("Configuration success")
    return True

def init():
    global cere_config
    if not os.path.isfile("cere.json"):
        logger.critical("No configuration file found. Run: cere configure or run this command from where cere.json is.")
        return False
    with open("cere.json", 'r') as config_file:
        cere_config = json.load(config_file)
    if not setup_dir():
        logger.critical("Cannot create required directories for CERE. Check permissions?")
        return False
    os.environ["CERE_WORKING_PATH"] = os.path.realpath(var.CERE_MAIN_DIR)
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
