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
import subprocess
import sys
from cere import cere_configure
import logging
import shutil
import argparse
from cere import vars as var
from cere.create_graph import create_graph

logger = logging.getLogger('Profile')

def init_module(subparsers, cere_plugins):
    cere_plugins["profile"] = run
    profile_parser = subparsers.add_parser("profile", help="Profiles an application")
    profile_parser.add_argument("--app", action='store_true', help="measures application cycles")
    profile_parser.add_argument("--regions", action="store_true", help="instruments regions and generates a call graph")
    profile_parser.add_argument('--force', '-f', action="store_true", help="overwrites any previous CERE measures. (default False)")

def run(args):
    if not cere_configure.init():
        return False

    if not args.app and not args.regions:
        args.app = True
        args.regions = True
    res_1=True
    res_2=True
    if args.regions:
        res_1 = instrument_application(cere_configure.cere_config["run_cmd"], cere_configure.cere_config["build_cmd"], cere_configure.cere_config["clean_cmd"], args.force)
    if args.app:
        res_2 = measure_application(cere_configure.cere_config["run_cmd"], cere_configure.cere_config["build_cmd"], cere_configure.cere_config["clean_cmd"], args.force)
    return (res_1 and res_2)

def measure_application(run_cmd, build_cmd, clean_cmd, force):
    logger.info('Measuring application cycles')
    if os.path.isfile("{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH)):
        if not force:
            logger.info('Keeping previous application cycles')
            return True
    try:
        env = dict(os.environ, CERE_MODE="original --instrument --wrapper={2}".format(var.RDTSC_WRAPPER))
        logger.debug(subprocess.check_output("{0} && {1}".format(clean_cmd, build_cmd), stderr=subprocess.STDOUT, shell=True, env=env))
        logger.info(subprocess.check_output(run_cmd, stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.critical(str(err))
        logger.critical(err.output)
    if not os.path.isfile("main.csv"):
        logger.critical('Measuring application failed: No output file')
        return False
    else:
        try:
            shutil.move("main.csv", "{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH))
        except IOError as err:
            logger.critical(str(err))
            return False
    logger.info('Measuring application success')
    return True

def instrument_application(run_cmd, build_cmd, clean_cmd, force):
    logger.info('Start instrumentation')
    if os.path.isfile("{0}/app.prof".format(var.CERE_PROFILE_PATH)) and not force:
        logger.info('Keeping previous instrumentation')
        create_graph(force)
        return True
    try:
        env = dict(os.environ, CERE_MODE="original --instrument --instrument-app")
        env["CPUPROFILE"] = "{0}/app.prof".format(var.CERE_PROFILE_PATH)
        logger.debug(subprocess.check_output("{0} && {1}".format(clean_cmd, build_cmd), stderr=subprocess.STDOUT, shell=True, env=env))
        logger.info(subprocess.check_output(run_cmd, stderr=subprocess.STDOUT, shell=True, env=env))
    except subprocess.CalledProcessError as err:
        logger.critical(str(err))
        logger.critical(err.output)
    if not os.path.isfile("{0}/app.prof".format(var.CERE_PROFILE_PATH)):
        logger.critical("Instrumentation failed: No output file")
        return False
    logger.info('Instrumentation success')
    create_graph(force)
    return True
