#!/usr/bin/env python

import os
import subprocess
import sys
import argparse
import logging
import subprocess
import cere_configure

def init_module(subparsers, cere_plugins):
    cere_plugins["dump"] = run
    dump_parser = subparsers.add_parser("dump", help="dump a region")
    dump_parser.add_argument('--region', help="Region to dump")
    dump_parser.add_argument('--invocation', type=int, default=1, help="invocation to dump (Default 1)")
    dump_parser.add_argument('--norun', type=bool, default=False, help="=If you don't want to automatically run the dump")
    dump_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-dump any previous CERE dumps")

def run(args):
    cere_configure.init()
    if(args.region):
        if not os.path.isdir("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation)) or args.force:
            logging.info("Compiling dump mode for region {0} invocation {1}".format(args.region, args.invocation))
            try:
                logging.debug(subprocess.check_output("{0} MODE=\"dump --region={1} --invocation={2}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, args.invocation), stderr=subprocess.STDOUT, shell=True))
            except subprocess.CalledProcessError as err:
                logging.critical(str(err))
                logging.critical(err.output)
                logging.info("Compiling dump mode for region {0} invocation {1} failed".format(args.region, args.invocation))
                return False
            if not args.norun:
                logging.info("Dumping invocation {1} for region {0}".format(args.region, args.invocation))
                try:
                    logging.debug(subprocess.check_output("LD_BIND_NOW=1 " + cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
                except subprocess.CalledProcessError as err:
                    #even if the dump run fails, maybe the region is dumped.
                    logging.debug(str(err))
                    logging.debug(err.output)
                if not os.path.isdir("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation)):
                    logging.critical("Dump failed for region {0} invocation {1}".format(args.region, args.invocation))
                    return False
                else:
                    logging.info("Invocation {1} succesfully dumped for region {0} ".format(args.region, args.invocation))
        else:
            logging.info("Dump already exists for region {0} invocation {1}".format(args.region, args.invocation))
    #Global dump
    else:
        logging.info("Compiling dump mode for all regions")
        try:
            logging.debug(subprocess.check_output("{0} MODE=\"dump\" -B".format(cere_configure.cere_config["build_cmd"]), stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.critical(str(err))
            logging.critical(err.output)
            logging.info("Compiling dump mode for all regions failed")
            return False
        if not args.norun:
            logging.info("Dumping all regions")
            try:
                logging.debug(subprocess.check_output("LD_BIND_NOW=1 " + cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
            except subprocess.CalledProcessError as err:
                #even if the dump run fails, maybe the region is dumped.
                logging.debug(str(err))
                logging.debug(err.output)
                logging.critical("Dumping all regions failed")
                return False
            else:
                logging.info("All regions succesfully dumped")
    return True
