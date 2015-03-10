#!/usr/bin/env python

import os
import sys
import shutil
import argparse
import logging
import subprocess
import common.variables as var
import cere_configure

def init_module(subparsers, cere_plugins):
    cere_plugins["instrument"] = run
    instrument_parser = subparsers.add_parser("instrument", help="Instrument a region in the original application")
    instrument_parser.add_argument('--region', required=True, help="Region to measure")
    instrument_parser.add_argument('--lib', default=var.RDTSC_LIB, help="Library used to instrument the loop (Default rdtsc)")
    instrument_parser.add_argument('--wrapper', default=var.RDTSC_WRAPPER, help="Wrapper used to make the link between cere interface and your library")
    instrument_parser.add_argument('--invocation', type=int, default=0, help="Invocation to measure (Default measures all)")
    instrument_parser.add_argument('--norun', const=True, default=False, nargs='?', help="=If you don't want to automatically run the measure")

def run(args):
    cere_configure.init()
    lib = "--lib={0}".format(args.lib)
    wrap = "--wrapper={0}".format(args.wrapper)
    mode=""
    if args.invocation != 0 :
        logging.info("Compiling measure mode for region {0} invocation {1}".format(args.region, args.invocation))
        mode = " --invocation={0}".format(args.invocation)
    else:
        logging.info("Compiling measure mode for region {0}".format(args.region))
    try:
        logging.debug(subprocess.check_output("{0} MODE=\"original --region={1} --instrument {2} {3} {4}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, mode, lib, wrap), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
        logging.info("Compiling region {0} failed".format(args.region))
        return False
    if not args.norun:
        logging.info("Measuring region {0}".format(args.region))
        try:
            logging.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.critical(str(err))
            logging.critical(err.output)
            logging.critical("Instrumentation failed for region {0}".format(args.region))
            return False
    return True
