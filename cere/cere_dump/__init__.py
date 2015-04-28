#!/usr/bin/env python

import os
import subprocess
import sys
import shutil
import argparse
import logging
import subprocess
import common.utils as utils
import cere_configure
import cere_io_checker

logger = logging.getLogger('Dump')

def init_module(subparsers, cere_plugins):
    cere_plugins["dump"] = run
    dump_parser = subparsers.add_parser("dump", help="captures a region")
    dump_parser.add_argument('--region', help="region to capture")
    dump_parser.add_argument('--invocation', type=int, default=1, help="invocation to capture (default 1)")
    dump_parser.add_argument('--norun', action='store_true', help="builds the capture-instrumented binary without running it")
    dump_parser.add_argument('--force', '-f', action='store_true', help="overwrites previous existing dump (default False)")

def run(args):
    if not cere_configure.init():
        return False

    if(args.region):
        if utils.is_invalid(args.region) and not args.force:
            logger.warning("{0} is invalid. Skipping dump".format(args.region))
            return False
        if os.path.isdir("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation)) and not args.force:
            logger.info("Dump already exists for region {0} invocation {1}".format(args.region, args.invocation))
            return True

        #Check IOs
        if not cere_io_checker.run(args):
            logger.warning("Checking IOs failed")

        shutil.rmtree("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation), ignore_errors=True)
        logger.info("Compiling dump mode for region {0} invocation {1}".format(args.region, args.invocation))
        try:
            logger.debug(subprocess.check_output("{0} MODE=\"dump --region={1} --invocation={2}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, args.invocation), stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logger.error(str(err))
            logger.error(err.output)
            logger.error("Compiling dump mode for region {0} invocation {1} failed".format(args.region, args.invocation))
            utils.mark_invalid(args.region)
            return False
        if not args.norun:
            logger.info("Dumping invocation {1} for region {0}".format(args.region, args.invocation))
            try:
                logger.info(subprocess.check_output("LD_BIND_NOW=1 " + cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
            except subprocess.CalledProcessError as err:
                #even if the dump run fails, maybe the region is dumped.
                logger.error(str(err))
                logger.error(err.output)
            if not os.path.isdir("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation)):
                logger.error("Dump failed for region {0} invocation {1}".format(args.region, args.invocation))
                utils.mark_invalid(args.region)
                return False
            else:
                logger.info("Invocation {1} succesfully dumped for region {0} ".format(args.region, args.invocation))
    #Global dump
    else:
        logger.info("Compiling dump mode for all regions")
        try:
            logger.debug(subprocess.check_output("{0} MODE=\"dump\" -B".format(cere_configure.cere_config["build_cmd"]), stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logger.error(str(err))
            logger.error(err.output)
            logger.info("Compiling dump mode for all regions failed")
            return False
        if not args.norun:
            logger.info("Dumping all regions")
            try:
                logger.debug(subprocess.check_output("LD_BIND_NOW=1 " + cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
            except subprocess.CalledProcessError as err:
                logger.error(str(err))
                logger.error(err.output)
                logger.error("Dumping all regions failed")
                return False
            else:
                logger.info("All regions succesfully dumped")
    return True
