#!/usr/bin/env python

import os
import logging
import subprocess
import copy
import common.variables as var
import common.utils as utils
import cere_configure
import cere_instrument

logger = logging.getLogger('Io_checker')

def init_module(subparsers, cere_plugins):
    cere_plugins["io-checker"] = run
    io_checker_parser = subparsers.add_parser("io-checker", help="Check if a region does IOs")
    io_checker_parser.add_argument('--region', required=True, help="Region to check")
    io_checker_parser.add_argument('--invocation', type=int, default=1, help="Invocation to measure (Default measures all)")

def run(args):
    # Make a local copy of args
    io_checker_args = copy.copy(args)
    io_checker_args.wrapper = var.IO_CHECKER_LIB
    io_checker_args.regions_file = None
    io_checker_args.norun = False
    io_checker_args.force = True

    if not cere_instrument.run(io_checker_args):
        return False

    #If strace has not generated a trace file it means there is no IO
    if not os.path.isfile("io_detection_trace"):
        return True
    # Parse the IO trace, if a read or write is detected, the region is invalid.
    with open("io_detection_trace", 'r') as io_trace:
        content = io_trace.readlines()
    for line in content:
        if line.startswith("read") or line.startswith("write"):
            op_type, expr_left = line.split("(", 1)
            fd, useless = expr_left.split(",", 1)
            # Writes on stdout and stderr are tolerated
            if (op_type == "write" and fd != 1 and fd != 2) \
            or (op_type == "read"):
                logger.info("Region {0}, invocation {1} is invalid because it does IOs".format(io_checker_args.region, io_checker_args.invocation))
                utils.mark_invalid(args.region)
                return True
    return True
