#!/usr/bin/env python

import os
import subprocess
import cere_configure
import logging
import csv
import argparse

def init_module(subparsers, cere_plugins):
    cere_plugins["check"] = run
    check_parser = subparsers.add_parser("check", help="Compare for a given region, the assembly between original loop and replay loop")
    check_parser.add_argument('--region', required=True, help="Region to check")

def compute_error(a, b):
    return (abs(a-b)/float(max(a, b)))*100

def run_shell_command(command):
    try:
        logging.debug(subprocess.check_output(command, stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
        logging.info("Fail: {0}".format(command))
        return False
    return True

def get_nlines(filename):
    #Objdump the object file
    if not run_shell_command("objdump -D {0} > cere_tmp".format(filename)):
        return False
    #count the number of lines
    try:
        n_lines = subprocess.Popen("wc -l cere_tmp", shell=True, stdout=subprocess.PIPE).communicate()[0].split()[0]
    except subprocess.CalledProcessError as err:
        logging.critical(str(err))
        logging.critical(err.output)
        logging.info("Fail: wc -l original")
        return False
    return int(n_lines)

def run(args):
    cere_configure.init()
    if not os.path.isfile("regions.csv"):
        logging.critical("Regions.csv file missing. Please run cere regions")
        return False
    filename = ""
    #Find the file where the region is
    with open("regions.csv") as regions_list:
        reader = csv.DictReader(regions_list)
        for row in reader:
                filename = row["File Name"]
                break
    filename = filename.replace(os.path.splitext(filename)[1], ".o")
    print("The file is {0}".format(filename))

    #Now let's compile it in the orginal application
    if not run_shell_command("{0} MODE=original -B".format(cere_configure.cere_config["build_cmd"])):
        return False
    original_lines = get_nlines(filename)
    if not original_lines:
        return False

    #Compile replay mode
    #we accept that compilation fails because the dump does not have to be present.
    try:
        logging.debug(subprocess.check_output("{0} MODE=\"replay --region={1}\" -B".format(cere_configure.cere_config["build_cmd"], args.region), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logging.debug(str(err))
        logging.debug(err.output)
        logging.debug("If the dump is not present, skip this error")
    replay_lines = get_nlines(filename)
    if not replay_lines:
        return False
    err = compute_error(original_lines, replay_lines)
    if err <= 15:
        logging.info("Assembly matching: Original lines = {0} && replay lines = {1} (error = {2})".format(original_lines, replay_lines, err))
    else:
        logging.info("Assembly not matching: Original lines = {0} && replay lines = {1} (error = {2})".format(original_lines, replay_lines, err))
    return True
