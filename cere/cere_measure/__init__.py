#!/usr/bin/env python

import os
import sys
import shutil
import argparse
import logging
import subprocess
import cere_configure

def init_module(subparsers, cere_plugins):
    cere_plugins["measure"] = run
    measure_parser = subparsers.add_parser("measure", help="Measure a region in the original application")
    measure_parser.add_argument('--region', required=True, help="Region to measure")
    measure_parser.add_argument('--tool', default="rdtsc", help="Tool used to instrument the loop (Default rdtsc)")
    measure_parser.add_argument('--invocation', type=int, default=0, help="Invocation to measure (Default measures all)")
    measure_parser.add_argument('--norun', const=True, default=False, nargs='?', help="=If you don't want to automatically run the measure")

def run(args):
    cere_configure.init()
    if(args.tool == "likwid" and args.trace):
        logging.info("Likwid does not support trace mode, continuing in non trace mode")
    mode = "--tool={0}".format(args.tool)
    if args.invocation != 0 :
        logging.info("Compiling measure mode for region {0} invocation {1}".format(args.region, args.invocation))
        mode = mode + " --invocation={0}".format(args.invocation)
    else:
        logging.info("Compiling measure mode for region {0}".format(args.region))
    try:
        logging.debug(subprocess.check_output("{0} MODE=\"original --region={1} --instrument {2}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, mode), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
        logging.info("Compiling for region {0} failed".format(args.region))
        return False
    if not args.norun:
        logging.info("Measuring region {0}".format(args.region))
        try:
            logging.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.critical(str(err))
            logging.critical(err.output)
            logging.critical("Measure failed for region {0}".format(args.region))
            return False
    return True
