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

import os
import subprocess
import cere_configure
import logging
import csv
import argparse

logger = logging.getLogger('ASM checker')

def init_module(subparsers, cere_plugins):
    cere_plugins["check"] = run
    check_parser = subparsers.add_parser("check", help="Compare for a given region, the assembly between original region and replay region")
    check_parser.add_argument("--max-error", default=15.0, help="Maximum tolerated error between original and replay regions (Default: 15%)")
    check_parser.add_argument('--region', required=True, help="Region to check")
    check_parser.add_argument('--path', help="Path of the object file")
    check_parser.add_argument("--diff-asm", nargs='?', const=True, default=False, help="Run vimdiff between original and replay file")

def compute_error(a, b):
    return (abs(a-b)/float(max(a, b)))*100

def run_shell_command(command):
    try:
        logger.debug(subprocess.check_output(command, stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.error("Fail: {0}".format(command))
        return False
    return True

def get_nlines(filename, functionname):
    #Objdump the function
    if not run_shell_command("gdb -batch -ex 'file {0}' -ex 'disassemble {1}' > cere_tmp".format(filename, functionname)):
        return False
    #count the number of lines
    try:
        n_lines = subprocess.Popen("wc -l cere_tmp", shell=True, stdout=subprocess.PIPE).communicate()[0].split()[0]
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.error("Fail: wc -l original")
        return False
    return int(n_lines)

def run(args):
    if not cere_configure.init():
        return False
    if not os.path.isfile("regions.csv"):
        logger.critical("Regions.csv file missing. Please run cere regions")
        return False
    region = args.region
    filename = ""
    functionname=""
    #Find the file where the region is
    with open("regions.csv") as regions_list:
        reader = csv.DictReader(regions_list)
        for row in reader:
            if region in row["Region Name"]:
                filename = row["File Name"]
                functionname = row["Function Name"]
                break
    if args.path:
        filename = filename.rsplit('/', 1)
        filename = args.path+"/"+filename[1]
    filename = filename.replace(os.path.splitext(filename)[1], ".o")
    logger.debug("The file is {0} and the function is {1}".format(filename, functionname))

    #Now let's compile it in the orginal application
    if not run_shell_command("{0} && {1} CERE_MODE=original".format(cere_configure.cere_config["clean_cmd"], cere_configure.cere_config["build_cmd"])):
        return False
    original_lines = get_nlines(filename, functionname)
    if not original_lines:
        return False
    #backup the original assembly file
    if not run_shell_command("cp cere_tmp cere_original"):
        return False

    #Compile replay mode
    #we accept that compilation fails because the dump does not have to be present.
    try:
        logger.debug(subprocess.check_output("{0} && {1} CERE_MODE=\"replay --region={2}\"".format(cere_configure.cere_config["clean_cmd"], cere_configure.cere_config["build_cmd"], args.region), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.warning("If the dump is not present, skip these warnings")
    replay_lines = get_nlines(filename, "run__cere__"+region)
    if not replay_lines:
        return False
    #backup the replay assembly file
    if not run_shell_command("mv cere_tmp cere_replay"):
        return False

    err = compute_error(original_lines, replay_lines)
    if err <= args.max_error:
        logger.info("Assembly matching: Original lines = {0} && replay lines = {1} (error = {2})".format(original_lines, replay_lines, err))
    else:
        logger.info("Assembly not matching: Original lines = {0} && replay lines = {1} (error = {2})".format(original_lines, replay_lines, err))
    if args.diff_asm:
        subprocess.call("vimdiff cere_original cere_replay", shell=True)
    return True
