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
import csv
import subprocess
import re
from cere import cere_configure
from cere import cere_capture
import logging
import shutil
import argparse
from cere import vars as var

logger = logging.getLogger('Regions')

def init_module(subparsers, cere_plugins):
  cere_plugins["regions"] = run
  profile_parser = subparsers.add_parser("regions", help="List extractible regions")
  profile_parser.add_argument("--static", action='store_true', help="List regions.")
  profile_parser.add_argument("--estimate-cycles", action='store_true', help="Add a column to the output CSV containing an estimation of each region's cycles number (computed from the coverage)")

def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath = program.split()

    for v in fpath:
        # Look in current dir
        if is_exe(v):
            return os.path.join(os.getcwd(), v)
        # Look in run path
        if is_exe(os.path.join(var.CERE_RUN_PATH, v)):
            return os.path.join(var.CERE_RUN_PATH, v)
        # Look in build path
        if is_exe(os.path.join(var.CERE_BUILD_PATH, v)):
            return os.path.join(var.CERE_BUILD_PATH, v)

    return None

def parse_line(regex_list, line):
  i=-1
  matchObj=""
  while not matchObj:
    try:
      i = i + 1
      matchObj = re.match( bytes(regex_list[i], "utf-8"), line )
    except IndexError:
      break
  return matchObj, i

def add_header(args, regions_file, new_regions_file):
  with open(regions_file, 'rt') as regions:
    r = csv.reader(regions)
    header = next(r)
    if args.estimate_cycles:
      header.append("Cycles Estimation")
  with open(new_regions_file, 'wt') as tmp_regions:
    w = csv.writer(tmp_regions)
    w.writerow(header)

def add_coverage(args, regions_file, new_regions_file, matchObj):
  name = matchObj.group(2)
  if bytes("__cere__", "utf-8") not in name:
    return

  try:
    self_coverage = float(matchObj.group(4))
  except IndexError:
    self_coverage = "NA"

  try:
      coverage = float(matchObj.group(6))
  except IndexError:
      coverage = self_coverage

  with open(regions_file, 'rt') as regions:
    r = csv.reader(regions)
    found=False
    for row in r:
      if bytes(row[0], "utf-8") == name:
        found=True
        break
  if(found):
    row[5] = self_coverage
    row[6] = coverage

    if args.estimate_cycles:
      cycles = int(args.app_cycles * coverage / 100)
      row.append(cycles)

    with open(new_regions_file, 'at') as tmp_regions:
      w = csv.writer(tmp_regions)
      w.writerow(row)

def run(args):
  if not cere_configure.init():
      return False
  profile_file = "{0}/app.prof".format(var.CERE_PROFILE_PATH)
  regions_file = os.path.join(var.CERE_WORKING_PATH, cere_configure.cere_config["regions_infos"])
  new_regions_file = "tmp.csv"
  build_cmd = cere_configure.cere_config["build_cmd"]
  run_cmd = cere_configure.cere_config["run_cmd"]
  clean_cmd = cere_configure.cere_config["clean_cmd"]

  logger.info("Removing previous regions list")
  if os.path.isfile(regions_file):
      os.remove(regions_file)

  #Build again the application to be sure we give the right binary to pprof
  try:
      env = dict(os.environ, CERE_MODE="original --instrument --instrument-app --regions-infos={0}".format(regions_file))
      logger.info(subprocess.check_output("{0} && {1}".format(clean_cmd, build_cmd),
                                          stderr=subprocess.STDOUT, shell=True, env=env, cwd=var.CERE_BUILD_PATH))
  except subprocess.CalledProcessError as err:
      logger.error(str(err))
      logger.error(err.output)
      return False
  if not os.path.isfile(regions_file):
    logger.critical("Failed to generate region list.")
    return False

  logger.info("Regions list dumped in {0} file".format(regions_file))

  if not args.static:
    if not os.path.isfile(profile_file):
      logger.critical('No profiling file. Please first run cere profile or run cere regions --static')
      return False

    binary = which(run_cmd)
    if not binary:
      logger.critical("Cannot find the binary. Please provide binary name through cere configure --binary")
      return False

    # If needed, retrieve app_cycles for cycles estimation
    if args.estimate_cycles :
      try:
        with open("{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH)) as app:
          reader = csv.DictReader(app)
          for row in reader:
            args.app_cycles = float(row["CPU_CLK_UNHALTED_CORE"])
      except :
        logger.error("Could not read total app cycles to estimate cycles. Disabling the option.")
        args.estimate_cycles = False


    add_header(args, regions_file, new_regions_file)

    #regular expression to parse the gperf tool output
    regex_list = [r'(N.*)\s\[label\=\"(.*?)\\n([0-9]*)\s\((.*)\%\)\\rof\s(.*)\s\((.*)\%\)\\r',
              r'(N.*)\s\[label\=\"(.*)\\n([0-9]*)\s\((.*)\%\)\\r',
              r'(N.*)\s\-\>\s(N.*)\s\[label\=([0-9]*)\,']

    cmd = subprocess.Popen("{0} -dot {1} {2}".format(var.PPROF, binary, profile_file), shell=True, stdout=subprocess.PIPE)

    for line in cmd.stdout:
      matchObj, step = parse_line(regex_list, line)
      if step < 2 :
        add_coverage(args, regions_file, new_regions_file, matchObj)
      else:
        continue

    try:
      shutil.move(new_regions_file, regions_file)
    except IOError as err:
      logger.critical(str(err))
      logger.error(err.output)
      return False

  return True
