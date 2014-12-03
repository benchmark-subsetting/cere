#!/usr/bin/env python

import os
import sys
import argparse
import logging
import cere_configure

def init_module(subparsers, cere_plugins):
    cere_plugins["dump"] = run
    dump_parser = subparsers.add_parser("dump", help="dump a region")
    dump_parser.add_argument('--region', required=True, help="Region to dump")
    dump_parser.add_argument('--invocation', type=int, default=1, help="invocation to dump (Default 1)")
    dump_parser.add_argument('--norun', type=bool, default=False, help="=If you don't want to automatically run the dump")
    dump_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-dump any previous CERE dumps")

def run(args):
    cere_configure.init()
    if not os.path.isdir("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation)) or args.force:
        ret = os.system("{0} MODE=\"dump --region={1} --invocation={2}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, args.invocation))
        if ret:
            logging.critical("{0} MODE=\"dump --region={1} --invocation={2}\" Failed".format(cere_configure.cere_config["build_cmd"], args.region, args.invocation))
            return False
        if not args.norun:
            ret = os.system("LD_BIND_NOW=1 " + cere_configure.cere_config["run_cmd"])
            if ret or not os.path.isdir("{0}/{1}/{2}".format(cere_configure.cere_config["cere_dumps_path"], args.region, args.invocation)):
                logging.critical("Dump failed for {0} invocation {1}".format(args.region, args.invocation))
                return False
            else:
                logging.info("Dump success for {0} invocation {1}".format(args.region, args.invocation))
    else:
        logging.info("Dump already exists for {0} invocation {1}".format(args.region, args.invocation))
    return True
