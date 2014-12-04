#!/usr/bin/env python

import os
import sys
import argparse
import logging
import subprocess
import cere_configure
import cere_dump

def init_module(subparsers, cere_plugins):
    cere_plugins["replay"] = run
    replay_parser = subparsers.add_parser("replay", help="replay a region")
    replay_parser.add_argument('--region', required=True, help="Region to replay")
    replay_parser.add_argument('--invocation', type=int, default=1, help="invocation to replay (Default 1)")
    replay_parser.add_argument('--noinstrumentation', type=bool, default=False, help="=If you don't want to instrument the region")
    replay_parser.add_argument('--norun', type=bool, default=False, help="=If you don't want to automatically run the region")
    replay_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-dump any previous CERE dumps")

def run(args):
    cere_configure.init()
    if not os.path.isdir("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation)) or args.force:
        #If the dump does not exist or we force the dump
        if not cere_dump.run(args):
            return False
    if args.noinstrumentation:
        instru_cmd = ""
        logging.info("Compiling replay mode invocation {0} for region {1} without instrumentation".format(args.region, args.invocation))
    else:
        instru_cmd = "--instrument"
        logging.info("Compiling replay mode invocation {0} for region {1} with instrumentation".format(args.region, args.invocation))

    logging.debug(subprocess.check_output("{0} MODE=\"replay --region={1} --invocation={2} {3}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, args.invocation, instru_cmd), stderr=subprocess.STDOUT, shell=True))

    if not args.norun:
        logging.info("Replaying invocation {1} for region {0}".format(args.region, args.invocation))
        logging.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        if not args.noinstrumentation:
            if not os.path.isfile("rdtsc_result.csv"):
                logging.critical("Replay failed for {0} invocation {1}".format(args.region, args.invocation))
                return False
    return True
