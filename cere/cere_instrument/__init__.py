#!/usr/bin/env python

import logging
import subprocess
import common.variables as var
import common.utils as utils
import cere_configure

logger = logging.getLogger('Instrument')

def init_module(subparsers, cere_plugins):
    cere_plugins["instrument"] = run
    instrument_parser = subparsers.add_parser("instrument", help="Instrument a region in the application")
    instrument_parser.add_argument('--region', help="Region to instrument")
    instrument_parser.add_argument('--regions-file', help="File containing the list of regions to instrument")
    instrument_parser.add_argument('--wrapper', default=var.RDTSC_WRAPPER, help="Wrapper used to make the link between cere interface and your library (Default: rdtsc)")
    instrument_parser.add_argument('--invocation', type=int, default=0, help="Invocation to measure (Default measures all)")
    instrument_parser.add_argument('--norun', action='store_true', help="=If you don't want to automatically run the measure")
    instrument_parser.add_argument('--force', '-f', action='store_true', help="Will force the CERE instrumentation")

def run_instrument(args_region=None, args_regions_file=None, args_wrapper=var.RDTSC_WRAPPER, args_invocation=0, args_norun=False, args_force=False):
    if not cere_configure.init():
        return False
    if utils.is_invalid(args_region) and not args_force:
        logger.error("{0} is invalid. Skipping instrumentation".format(args_region))
        return False

    if args_regions_file:
        region_input = "--regions-file={0}".format(args_regions_file)
    elif args_region:
        region_input = "--region={0}".format(args_region)
    else:
        logger.error("No region specified, use at least one of the following: --region, --regions-file")
        return False

    if args_invocation != 0 :
        logger.info("Compiling instrumentation mode for {0} invocation {1}".format(region_input, args_invocation))
        mode = " --invocation={0}".format(args_invocation)
    else:
        logger.info("Compiling instrumentation mode for {0}".format(region_input))
        mode = ""

    try:
        logger.debug(subprocess.check_output("{0} MODE=\"original {1} --instrument {2} --wrapper={3}\" -B".format(cere_configure.cere_config["build_cmd"], region_input, mode, args_wrapper), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.error("Compiling instrumentation failed")
        return False

    if not args_norun:
        logger.info("Running instrumentation for {0}".format(region_input))
        try:
            logger.info(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logger.error(str(err))
            logger.error(err.output)
            logger.error("Instrumentation failed")
            return False
    return True

def run(args):
    return run_instrument(args.region, args.regions_file, args.wrapper, args.invocation, args.norun, args.force)
