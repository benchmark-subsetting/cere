#!/usr/bin/env python

import os
import sys
import shutil
import argparse
import logging
import subprocess
import cere_configure
import common.variables as var

def init_module(subparsers, cere_plugins):
    cere_plugins["trace"] = run
    replay_parser = subparsers.add_parser("trace", help="trace a region")
    replay_parser.add_argument('--region', required=True, help="Region to trace")
    replay_parser.add_argument('--norun', type=bool, const=True, default=False, nargs='?', help="=If you don't want to automatically run the trace")
    replay_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-trace any previous CERE trace")

def run(args):
    cere_configure.init()
    if not os.path.isfile("{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], args.region))\
    or not os.path.isfile("{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], args.region))\
    or args.force:
        logging.info("Compiling trace mode for region {0}".format(args.region))
        try:
            logging.debug(subprocess.check_output("{0} MODE=\"original --region={1} --instrument --trace --lib={2} --wrapper={3}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, var.RDTSC_LIB, var.RDTSC_WRAPPER), stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.critical(str(err))
            logging.critical(err.output)
            logging.info("Compiling trace mode for region {0} failed".format(args.region))
            return False
        if not args.norun:
            logging.info("Tracing region {0}".format(args.region))
            try:
                logging.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
            except subprocess.CalledProcessError as err:
                logging.critical(str(err))
                logging.critical(err.output)
                logging.critical("Trace failed for region {0}".format(args.region))
                return False
            try:
                shutil.move("{0}.bin".format(args.region), "{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], args.region))
                shutil.move("rdtsc_result.csv", "{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], args.region))
            except IOError as err:
                logging.critical(str(err))
                logging.critical("Trace failed for region {0}: No output files, maybe the selected region does not exist.".format(args.region))
                return False
    else:
        logging.info("Trace already exists for region {0}".format(args.region))
    return True
