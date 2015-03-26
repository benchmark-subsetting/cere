#!/usr/bin/env python

import os
import csv
import subprocess
import re
import cere_configure
import cere_dump
import logging
import shutil
import argparse

def init_module(subparsers, cere_plugins):
    cere_plugins["regions"] = run
    profile_parser = subparsers.add_parser("regions", help="List extractible loops")
    profile_parser.add_argument("--static", nargs='?', const=True, default=True, help="List loops")
    profile_parser.add_argument("--dynamic", nargs='?', const=True, default=False, help="List loops with coverage")

class Dump():
    def __init__(self):
        self.force = False
        self.norun = True
        self.region = False

def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath = program.split()
    for v in fpath:
        if is_exe(v):
            return v
    return None

def parse_line(regex_list, line):
    i=-1
    matchObj=""
    while not matchObj:
        try:
            i = i + 1
            matchObj = re.match( regex_list[i], line )
        except IndexError:
            break
    return matchObj, i

def add_column_to_header(regions_file, new_regions_file):
    with open(regions_file, 'rb') as regions:
        r = csv.reader(regions)
        header = r.next()
        header.append('Coverage (self)')
        header.append('Coverage')
    with open(new_regions_file, 'wb') as tmp_regions:
        w = csv.writer(tmp_regions)
        w.writerow(header)

def add_coverage(regions_file, new_regions_file, matchObj):
    name = matchObj.group(2)
    if "__cere__"  not in name: return

    try:
        coverage = float(matchObj.group(6))
    except IndexError:
        coverage = 0.0

    try:
        self_coverage = float(matchObj.group(4))
    except IndexError:
        self_coverage = 0.0

    with open(regions_file, 'rb') as regions:
        r = csv.reader(regions)
        found=False
        for row in r:
            if row[0] == name:
                found=True
                break
    if(found):
        row.append(self_coverage)
        row.append(coverage)
        with open(new_regions_file, 'ab') as tmp_regions:
            w = csv.writer(tmp_regions)
            w.writerow(row)

def run(args):
    cere_configure.init()
    profile_file = "{0}/app.prof".format(cere_configure.cere_config["cere_measures_path"])
    regions_file = "regions.csv"
    new_regions_file = "tmp.csv"

    logging.info("Removing previous regions list")
    if os.path.isfile(regions_file):
        os.remove(regions_file)
    if(args.static):
        mydump = Dump()
        res = cere_dump.run(mydump)
        logging.info("Regions list dumped in regions.csv file")
        if not res:
            logging.critical("Regions listing encountered and error: list may be incomplete or empty")
            return False
    if(args.dynamic):
        if not os.path.isfile(profile_file):
            logging.critical('No profiling file. Please run cere profile --regions')
            return False
        if not os.path.isfile(regions_file):
            logging.critical('Regions file does not exists. Please re-run cere regions with static enabled.')
            return False

        build_cmd = cere_configure.cere_config["build_cmd"]
        run_cmd = cere_configure.cere_config["run_cmd"]

        binary = which(run_cmd)
        if not binary:
            logging.critical("Can't find the binary")
            return False

        add_column_to_header(regions_file, new_regions_file)

        #regular expression to parse the gperf tool output
        regex_list = [r'(N.*)\s\[label\=\"(.*?)\\n([0-9]*)\s\((.*)\%\)\\rof\s(.*)\s\((.*)\%\)\\r',
                  r'(N.*)\s\[label\=\"(.*)\\n([0-9]*)\s\((.*)\%\)\\r',
                  r'(N.*)\s\-\>\s(N.*)\s\[label\=([0-9]*)\,']

        #Build again the application to be sure we give the right binary to pprof
        try:
            logging.info(subprocess.check_output("{0} MODE=\"original --instrument --instrument-app\" -B".format(build_cmd), stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logging.critical(str(err))
            logging.critical(err.output)
            return False
        cmd = subprocess.Popen("pprof -dot {0} {1}".format(binary, profile_file), shell=True, stdout=subprocess.PIPE)

        for line in cmd.stdout:
            matchObj, step = parse_line(regex_list, line)
            if step < 2 :
                add_coverage(regions_file, new_regions_file, matchObj)
            else:
                continue
        try:
            shutil.move(new_regions_file, regions_file)
        except IOError as err:
            logging.critical(str(err))
            return False
    return True
