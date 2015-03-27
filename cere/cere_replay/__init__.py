#!/usr/bin/env python

import os
import sys
import argparse
import logging
import shutil
import subprocess
import cere_configure
import cere_dump
import common.variables as var
import common.utils as utils

def init_module(subparsers, cere_plugins):
    cere_plugins["replay"] = run
    replay_parser = subparsers.add_parser("replay", help="replay a region")
    replay_parser.add_argument('--region', required=True, help="Region to replay")
    replay_parser.add_argument('--invocation', type=int, default=1, help="invocation to replay (Default 1)")
    replay_parser.add_argument('--invitro-callcount', type=int, default=10, help="Meta-repetition for the replay (Default 10)")
    replay_parser.add_argument('--noinstrumentation', type=bool, const=True, default=False, nargs='?', help="=If you don't want to instrument the region")
    replay_parser.add_argument('--lib', default=var.RDTSC_LIB, help="Library used to instrument the loop (Default rdtsc)")
    replay_parser.add_argument('--wrapper', default=var.RDTSC_WRAPPER, help="Wrapper used to make the link between cere interface and your library")
    replay_parser.add_argument('--norun', type=bool, default=False, help="=If you don't want to automatically run the region")
    replay_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-dump any previous CERE dumps")

def run(args):
    cere_configure.init()
    if utils.is_invalid(args.region):
        logging.error("{0} is invalid".format(args.region))
        return False
    if os.path.isfile("{0}/{1}_{2}.csv".format(cere_configure.cere_config["cere_measures_path"], args.region, args.invocation)) and not args.force:
        logging.info("Keeping previous replay measures for {0} invocation {1}.".format(args.region, args.invocation))
        return True
    #If the dump does not exist
    if not cere_dump.run(args):
        return False
    if args.noinstrumentation:
        instru_cmd = ""
        lib = ""
        wrap = ""
        logging.info("Compiling replay mode for region {0} invocation {1} without instrumentation".format(args.region, args.invocation))
    else:
        instru_cmd = "--instrument"
        lib = "--lib={0}".format(args.lib)
        wrap = "--wrapper={0}".format(args.wrapper)
        logging.info("Compiling replay mode for region {0} invocation {1} with instrumentation".format(args.region, args.invocation))

    try:
        logging.debug(subprocess.check_output("{0} INVITRO_CALL_COUNT={6} MODE=\"replay --region={1} --invocation={2} {3} {4} {5}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, args.invocation, instru_cmd, lib, wrap, args.invitro_callcount), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
        logging.info("Compiling replay mode for region {0} invocation {1} Failed".format(args.region, args.invocation))
        utils.mark_invalid(args.region)
        return False
    if not args.norun:
        logging.info("Replaying invocation {1} for region {0}".format(args.region, args.invocation))
        try:
            logging.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.critical(str(err))
            logging.critical(err.output)
            logging.critical("Replay failed for {0} invocation {1}".format(args.region, args.invocation))
            utils.mark_invalid(args.region)
            return False
    return True
