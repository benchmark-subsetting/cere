#!/usr/bin/env python

import os
import shutil
import logging
import subprocess
import copy
import tempfile
import re
import cere_configure
import common.variables as var
import common.utils as utils
import common.errors as cere_error
import cere_instrument

logger = logging.getLogger('Check-io')
tolerated_fd = [1,2]
#This list must match operations traced by strace in io_detector.c
forbidden_ios = ["read", "write", "mmap"]

def init_module(subparsers, cere_plugins):
    cere_plugins["check-io"] = run
    io_checker_parser = subparsers.add_parser("io-checker", help="Check if a region does IOs")
    io_checker_parser.add_argument('--region', help="Region to check")
    io_checker_parser.add_argument('--regions-file', help="File containing the list of regions to check")
    io_checker_parser.add_argument('--invocation', type=int, default=1, help="Invocation to measure (Default measure the first)")
    io_checker_parser.add_argument('--force', '-f', action='store_true', help="overwrites previous existing dump (default False)")

def run_io_checker(args_region=None, args_regions_file=None, args_invocation=1, args_force=False):
    if not cere_configure.init():
        return False

    if not check_arguments(args_region, args_regions_file, args_invocation, args_force):
        return False

    if args_regions_file:
        # Make regions_file path absolute
        args_regions_file = os.path.abspath(args_regions_file)

    regions_to_trace = find_regions_to_trace(args_region, args_regions_file, args_invocation, args_force)
    if len(regions_to_trace) == 0:
        logger.info("No regions to trace")
    else:
        if not launch_io_trace(args_force, regions_to_trace):
            return False

    parse_io_trace(regions_to_trace)

    return True

def launch_io_trace(args_force, regions_to_check):
    with tempfile.NamedTemporaryFile() as temp:
        for region, invocations in regions_to_check.iteritems():
            temp.write(region)
            for invocation in invocations:
                temp.write(' ' + str(invocation))
            temp.write('\n')
        temp.flush()

        logger.info("Checking IOs")
        if not cere_instrument.run_instrument(None, temp.name, var.IO_CHECKER_LIB, 0, False, args_force):
            return False
    return True

def check_arguments(args_region, args_regions_file, args_invocation, args_force):
    """
    Check arguments
    """

    if not (args_region or args_regions_file):
        logger.error("No region specified, use at least one of the following: --region, --regions-file")
        return False

    if (args_regions_file and args_region):
        logger.error("--region and --regions-file are exclusive")
        return False

    if args_regions_file:
        if not os.path.isfile(args_regions_file):
            logger.error("No such file: {0}".format(args_regions_file))
            return False

    return True

def find_regions_to_trace(args_region, args_regions_file, args_invocation, args_force):
    regions_to_check = {}

    # If user passed a regions_file, read regions from it
    if args_regions_file:
        with file(args_regions_file) as f:
            for line in f.readlines():
                # Trim line
                line = line.strip()
                regions_info = line.split()
                region = regions_info[0]
                invocations = regions_info[1:]
                if not invocations:
                    invocations = [1]
                # If the IO trace already exists, we can skip the test
                if not args_force:
                    invocations = [invocation for invocation in invocations if not utils.io_trace_exists(region, invocation)]
                if invocations:
                    regions_to_check[region] = invocations
    # If user passed a single region
    elif args_region:
        # If the IO trace already exists, we can skip the test
        if utils.io_trace_exists(args_region, args_invocation) and not args_force:
            logger.info("IO detection already done for {0} invocation {1}".format(args_region, args_invocation))
        else:
            regions_to_check[args_region] = [args_invocation]

    return regions_to_check

def parse_io_trace(regions_to_trace):
    missing_trace = []
    file_stack = []
    region_stack = []
    for region, invocations in regions_to_trace.iteritems():
        for invocation in invocations:
            trace_dir = os.path.join(var.CERE_IO_TRACES_PATH, region, str(invocation), region)
            #If strace has not generated a trace file it means:
            #there is no IO or the trace is included in another trace
            if not os.path.isfile(trace_dir):
                missing_trace.append(trace_dir)
                continue
            region_stack.append(region)
            regex = r'(.*)\(([0-9]*)\,\s\"(.*)\"'
            # Parse the IO trace, if a read or write is detected, the region is invalid.
            with open(trace_dir, 'r') as io_trace:
                content = io_trace.readlines()
            for line in content:
                matchObj = re.match(regex, line)
                if not matchObj: continue
                op_type = matchObj.group(1)
                fd = int(matchObj.group(2))
                text = matchObj.group(3)
                if "CERE_" in text:
                    nested_region = text.split()[1]
                    nested_invocation = text.split()[2].replace("\\n", "")
                    #Starting a nested region
                    if "CERE_START" in text:
                        #create the region trace
                        trace_file = os.path.join(var.CERE_IO_TRACES_PATH, nested_region, nested_invocation, nested_region)
                        nested_file = open(trace_file, 'a')
                        #remove it from the missing trace
                        if trace_file in missing_trace:
                            missing_trace.remove(trace_file)
                        file_stack.append(nested_file)
                        region_stack.append(nested_region)
                    elif "CERE_STOP" in text:
                        #Close the nested region trace
                        file_stack.pop().close()
                        region_stack.pop()
                else:
                    for f in file_stack:
                        f.write(line)
                    if (op_type in forbidden_ios) and (fd not in tolerated_fd):
                        for r in region_stack:
                            utils.mark_invalid(r, cere_error.EIO)
            region_stack.pop()
    for r in region_stack:
        utils.mark_invalid(r, "Failed to run IO checker")

def run(args):
    return run_io_checker(args.region, args.regions_file, args.invocation, args.force)
