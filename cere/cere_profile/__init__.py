#!/usr/bin/env python

import os
import subprocess
import sys
import cere_configure
import logging
import shutil
import argparse

def init_module(subparsers, cere_plugins):
    cere_plugins["profile"] = run
    profile_parser = subparsers.add_parser("profile", help="Profile an application")
    profile_parser.add_argument("--app", nargs='?', const=True, default=False, help="Measure application cycles")
    profile_parser.add_argument("--regions", nargs='?', const=True, default=False, help="Instrument regions and generates call graph")
    profile_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will overwrite any previous CERE measures")

def run(args):
    cere_configure.init()

    if not args.app and not args.regions:
        args.app = True
        args.regions = True
    if args.regions:
        instrument_application(cere_configure.cere_config["run_cmd"], cere_configure.cere_config["build_cmd"], cere_configure.cere_config["cere_measures_path"], args.force)
    if args.app:
        measure_application(cere_configure.cere_config["run_cmd"], cere_configure.cere_config["build_cmd"], cere_configure.cere_config["cere_measures_path"], args.force)
    return True

def measure_application(run_cmd, build_cmd, measures_path, force):
    logging.info('Measuring application cycles')
    if os.path.isfile("{0}/app_cycles.csv".format(measures_path)):
        if not force:
            logging.info('Keeping previous application cycles')
            return True
    try:
        logging.info(subprocess.check_output("{0} MODE=\"original --instrument\" -B".format(build_cmd), stderr=subprocess.STDOUT, shell=True))
        logging.info(subprocess.check_output(run_cmd, stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
    if not os.path.isfile("rdtsc_result.csv"):
        logging.critical('Measuring application failed: No output file')
        return False
    else:
        try:
            shutil.move("rdtsc_result.csv", "{0}/app_cycles.csv".format(measures_path))
        except IOError as err:
            logging.critical(str(err))
            return False
    logging.info('Measuring application success')
    return True

def instrument_application(run_cmd, build_cmd, measures_path, force):
    logging.info('Start instrumentation')
    if os.path.isfile("{0}/app.prof".format(measures_path)):
        if not force:
            logging.info('Keeping previous instrumentation')
            return True
    try:
        logging.info(subprocess.check_output("{0} MODE=\"original --instrument --instrument-app\" -B".format(build_cmd), stderr=subprocess.STDOUT, shell=True))
        logging.info(subprocess.check_output("CPUPROFILE={0}/app.prof {1}".format(measures_path, run_cmd), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
    if not os.path.isfile("{0}/app.prof".format(measures_path)):
        logging.critical("Instrumentation failed: No output file")
        return False
    logging.info('Instrumentation success')
    return True
