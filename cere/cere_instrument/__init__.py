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
    instrument_parser.add_argument('--region', required=True, help="Region to instrument")
    instrument_parser.add_argument('--regions-file', help="File containing the list of regions to instrument")
    instrument_parser.add_argument('--wrapper', default=var.RDTSC_WRAPPER, help="Wrapper used to make the link between cere interface and your library (Default: rdtsc)")
    instrument_parser.add_argument('--invocation', type=int, default=0, help="Invocation to measure (Default measures all)")
    instrument_parser.add_argument('--norun', const=True, default=False, nargs='?', help="=If you don't want to automatically run the measure")
    instrument_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will force the CERE instrumentation")

def run(args):
    cere_configure.init()
    if utils.is_invalid(args.region) and not args.force:
        logger.error("{0} is invalid. Skipping instrumentation".format(args.region))
        return False

    if args.regions_file:
        region_input = "--regions-file={0}".format(args.regions_file)
    else:
        region_input = "--region={0}".format(args.region)

    if args.invocation != 0 :
        logger.info("Compiling instrumentation mode for {0} invocation {1}".format(region_input, args.invocation))
        mode = " --invocation={0}".format(args.invocation)
    else:
        logger.info("Compiling instrumentation mode for {0}".format(region_input))
        mode = ""

    try:
        logger.debug(subprocess.check_output("{0} MODE=\"original {1} --instrument {2} --wrapper={3}\" -B".format(cere_configure.cere_config["build_cmd"], region_input, mode, args.wrapper), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.error("Compiling instrumentation failed")
        utils.mark_invalid(args.region)
        return False

    if not args.norun:
        logger.info("Running instrumentation for {0}".format(region_input))
        try:
            logger.info(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logger.error(str(err))
            logger.error(err.output)
            logger.error("Instrumentation failed")
            utils.mark_invalid(args.region)
            return False
    return True
