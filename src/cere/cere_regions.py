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
import cere_configure
import cere_capture
import logging
import shutil
import argparse
import vars as var
from graph_utils import load_graph

logger = logging.getLogger('Regions')

def init_module(subparsers, cere_plugins):
  cere_plugins["regions"] = run
  profile_parser = subparsers.add_parser("regions", help="List extractible regions")
  profile_parser.add_argument("--static", action='store_true', help="List regions.")

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

def add_header(regions_file, new_regions_file):
  with open(regions_file, 'rb') as regions:
    r = csv.reader(regions)
    header = r.next()
  with open(new_regions_file, 'wb') as tmp_regions:
    w = csv.writer(tmp_regions)
    w.writerow(header)

def add_coverage(regions_file, new_regions_file, d):
  with open(regions_file, 'rb') as regions:
    r = csv.reader(regions)
    found=False
    for row in r:
      if row[0] == d['_name']:
        found=True
        break
  if(found):
    row[5] = d['_self_coverage']
    row[6] = d['_coverage']
    with open(new_regions_file, 'ab') as tmp_regions:
      w = csv.writer(tmp_regions)
      w.writerow(row)

def run(args):
  if not cere_configure.init():
      return False
  profile_file = "{0}/app.prof".format(var.CERE_PROFILE_PATH)
  regions_file = cere_configure.cere_config["regions_infos"]
  new_regions_file = "tmp.csv"
  graph = load_graph()
  build_cmd = cere_configure.cere_config["build_cmd"]
  run_cmd = cere_configure.cere_config["run_cmd"]
  clean_cmd = cere_configure.cere_config["clean_cmd"]

  logger.info("Removing previous regions list")
  if os.path.isfile(regions_file):
      os.remove(regions_file)

  #Build again the application to be sure we give the right binary to pprof
  try:
      logger.info(subprocess.check_output("{0} && {1} CERE_MODE=\"original --instrument --instrument-app --regions-infos={2}\"".format(clean_cmd, build_cmd, regions_file), stderr=subprocess.STDOUT, shell=True))
  except subprocess.CalledProcessError as err:
      logger.error(str(err))
      logger.error(err.output)
      return False
  if not os.path.isfile(regions_file):
    logger.critical("Failed to generate region list.")
    return False

  logger.info("Regions list dumped in {0} file".format(regions_file))

  if not args.static:
    if not os.path.isfile(profile_file) or not graph:
      logger.warning('No profiling file. Cannot add coverage for regions. Please first run cere profile and then run cere regions again')
      return False

    binary = which(run_cmd)
    if not binary:
      logger.critical("Cannot find the binary. Please provide binary name through cere configure --binary")
      return False

    add_header(regions_file, new_regions_file)

    for n, d in graph.nodes(data=True):
      add_coverage(regions_file, new_regions_file, d)

    try:
      shutil.move(new_regions_file, regions_file)
    except IOError as err:
      logger.critical(str(err))
      logger.error(err.output)
      return False
  return True
