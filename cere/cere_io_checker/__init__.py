#!/usr/bin/env python

import os
import shutil
import logging
import subprocess
import copy
import common.variables as var
import common.utils as utils
import cere_configure
import cere_instrument

logger = logging.getLogger('Io_checker')
tolerated_fd = [1,2]

def init_module(subparsers, cere_plugins):
    cere_plugins["io-checker"] = run
    io_checker_parser = subparsers.add_parser("io-checker", help="Check if a region does IOs")
    io_checker_parser.add_argument('--region', required=True, help="Region to check")
    io_checker_parser.add_argument('--invocation', type=int, default=1, help="Invocation to measure (Default measures all)")
    io_checker_parser.add_argument('--force', '-f', action='store_true', help="overwrites previous existing dump (default False)")

def run(args):
    if not cere_configure.init():
        return False

    if utils.is_invalid(args.region):
        return True

    # If the IO trace already exists, we can skip the test
    dump_dir = os.path.join(cere_configure.cere_config["cere_dumps_path"], args.region, str(args.invocation))
    if os.path.isfile("{0}/io_detection_trace".format(dump_dir)) and not args.force:
        logger.info("IO detection already done")
        return True

    # Create the dump dir
    if not os.path.isdir(dump_dir):
        try:
            os.makedirs(dump_dir)
        except (OSError) as err:
            logger.error(str(err))
            logger.err("Cannot create {0}".format(dump_dir))
            return False
    try:
        os.remove("{0}/io_detection_trace".format(dump_dir))
    except OSError:
        pass

    # Make a local copy of args
    io_checker_args = copy.copy(args)
    io_checker_args.wrapper = var.IO_CHECKER_LIB
    io_checker_args.regions_file = None
    io_checker_args.norun = False

    logger.info("Checking IOs")
    if not cere_instrument.run(io_checker_args):
        return False

    #If strace has not generated a trace file it means there is no IO
    if not os.path.isfile("io_detection_trace"):
        return True

    # Move the IO trace to the dump dir
    shutil.move("io_detection_trace", dump_dir)

    # Parse the IO trace, if a read or write is detected, the region is invalid.
    with open("{0}/io_detection_trace".format(dump_dir), 'r') as io_trace:
        content = io_trace.readlines()
    for line in content:
        if line.startswith("read") or line.startswith("write"):
            op_type, expr_left = line.split("(", 1)
            fd  = int(expr_left.split(",", 1)[0])
            # Writes on stdout and stderr are tolerated
            if (op_type == "write" and (fd not in tolerated_fd)) \
            or (op_type == "read"):
                logger.info("Region {0}, invocation {1} is invalid because it does IOs".format(io_checker_args.region, io_checker_args.invocation))
                utils.mark_invalid(args.region)
                return True
    return True
