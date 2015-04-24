#!/usr/bin/env python

import os
import logging
import subprocess
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
    args.wrapper = var.IO_CHECKER_LIB
    args.regions_file = None
    args.norun = False
    args.force = True

    if not cere_instrument.run(args):
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
                logger.info("Region {0}, invocation {1} is invalid because it does IOs".format(args.region, args.invocation))
                utils.mark_invalid(args.region)
                return True
    return True
