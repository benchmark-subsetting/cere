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

import logging
import subprocess
import vars as var
import utils
import cere_configure

logger = logging.getLogger('Instrument')

def init_module(subparsers, cere_plugins):
    cere_plugins["instrument"] = run
    instrument_parser = subparsers.add_parser("instrument", help="Instrument a region in the application")
    instrument_parser.add_argument('--region', help="Region to instrument")
    instrument_parser.add_argument('--regions-file', help="File containing the list of regions to instrument")
    instrument_parser.add_argument('--plugin-instr', default=var.RDTSC_WRAPPER, help="Plugin to instrument the region")
    instrument_parser.add_argument('--invocation', type=int, default=0, help="Invocation to measure (Default measures all)")
    instrument_parser.add_argument('--norun', action='store_true', help="If you don't want to automatically run the measure")
    instrument_parser.add_argument('--force', '-f', action='store_true', help="Will force the CERE instrumentation")

def run_instrument(args_region=None, args_regions_file=None, args_plugin_instr=var.RDTSC_WRAPPER, args_invocation=0, args_norun=False, args_force=False):
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
        logger.debug(subprocess.check_output("{0} && {1} CERE_MODE=\"original {2} --instrument {3} --wrapper={4}\"".format(cere_configure.cere_config["clean_cmd"],
                                              cere_configure.cere_config["build_cmd"], region_input, mode, args_plugin_instr), stderr=subprocess.STDOUT, shell=True))
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
    return run_instrument(args.region, args.regions_file, args.plugin_instr, args.invocation, args.norun, args.force)
