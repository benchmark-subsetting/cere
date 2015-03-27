#!/usr/bin/env python

import os
import sys
import shutil
import argparse
import logging
import subprocess
import common.variables as var
import common.utils as utils
import cere_configure

def init_module(subparsers, cere_plugins):
    cere_plugins["instrument"] = run
    instrument_parser = subparsers.add_parser("instrument", help="Instrument a region in the original application")
    instrument_parser.add_argument('--region', required=True, help="Region to instrument")
    instrument_parser.add_argument('--regions-file', help="File containing the list of regions to instrument")
    instrument_parser.add_argument('--wrapper', default=var.RDTSC_WRAPPER, help="Wrapper used to make the link between cere interface and your library")
    instrument_parser.add_argument('--invocation', type=int, default=0, help="Invocation to measure (Default measures all)")
    instrument_parser.add_argument('--norun', const=True, default=False, nargs='?', help="=If you don't want to automatically run the measure")
    instrument_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will force the CERE instrumentation")

def run(args):
    cere_configure.init()
    if utils.is_invalid(args.region) and not args.force:
        logging.error("{0} is invalid".format(args.region))
        return False
    region_input = ""
    if args.regions_file:
        region_input = "--regions-file={0}".format(args.regions_file)
    elif cere_configure.cere_config["multiple_trace"] and args.trace_file:
        region_input = "--regions-file={0}".format(args.trace_file)
    else:
        region_input = "--region={0}".format(args.region)
    mode=""
    if args.invocation != 0 :
        logging.info("Compiling instrumentation mode for {0} invocation {1}".format(region_input, args.invocation))
        mode = " --invocation={0}".format(args.invocation)
    else:
        logging.info("Compiling instrumentation mode for {0}".format(region_input))
    try:
        logging.debug(subprocess.check_output("{0} MODE=\"original {1} --instrument {2} --wrapper={3}\" -B".format(cere_configure.cere_config["build_cmd"], region_input, mode, args.wrapper), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.debug(str(err))
        logging.debug(err.output)
        logging.critical("Compiling instrumentation failed")
        utils.mark_invalid(args.region)
        return False
    if not args.norun:
        logging.info("Instrumentation for {0}".format(region_input))
        try:
            logging.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.debug(str(err))
            logging.debug(err.output)
            logging.info("Instrumentation failed")
            utils.mark_invalid(args.region)
            return False
    return True
