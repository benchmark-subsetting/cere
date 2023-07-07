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
import sys
import shutil
import argparse
import logging
import copy
import subprocess
from cere import utils
from cere import vars as var
from cere import errors as cere_error
from cere import cere_configure

logger = logging.getLogger('Capture')

def init_module(subparsers, cere_plugins):
    cere_plugins["capture"] = run
    capture_parser = subparsers.add_parser("capture", help="captures a region")
    capture_parser.add_argument('--region', required=True, help="list of regions to capture, \";\" separated)")
    capture_parser.add_argument('--invocation', type=int, help="list of invocation to capture, \",\" separated for invocations of a same region and \";\" separated for invocations of different regions")
    capture_parser.add_argument('--norun', action='store_true', help="builds the capture-instrumented binary without running it")
    capture_parser.add_argument('--force', '-f', action='store_true', help="overwrites previous existing dump (default False)")

def find_invocations(chosen_invoc, regions):
  print("[find_invocations] In")
  print("[find_invocations] chosen_invoc=", chosen_invoc)
  print("[find_invocations] regions=", regions)

  all_invocations = []

  for region in regions:
    tmp_invocations = []
    if not chosen_invoc:
      if not os.path.isfile("{0}/{1}.invocations".format(var.CERE_TRACES_PATH, region)):
        logger.error("Representative invocations file is missing.\n\
                          Please run cere selectinv --region={0}\n\
                          Or choose manually an invocation with --invocation".format(region))
        return None
      with open("{0}/{1}.invocations".format(var.CERE_TRACES_PATH, region)) as invocation_file:
        invitro_cycles=0.
        for line in invocation_file:
          infos = line.strip().split()
          tmp_invocations.append(infos[0])
    else:
      tmp_invocations.append(chosen_invoc)

    all_invocations.append(tmp_invocations)


  print("[find_invocations] Returning all_invocations=", all_invocations)
  return all_invocations

def run(args):
  if not cere_configure.init():
    return False

  regions = args.region.split(";")
  invocations = find_invocations(args.invocation, regions)
  if not invocations:
    return False

  # Check regions arg
  for i in range(len(regions)):
    if not args.force:

      # Checking if any of the region/invocation couple exists
      for j in (range(len(invocations[i]))):
        if utils.dump_exist(regions[i], invocations[i][j]):
          logger.info("Dump already exists for region {0} invocation {1}".format(regions[i], invocations[i][j]))
          return False

      # Checking if the region is invalid
      if utils.is_invalid(regions[i]):
        logger.warning("{0} is invalid. Skipping capture".format(regions[i]))
        return False


    else:
      # Else, if overwrite is enabled, remove all previous dumps of requested regions/invocations
      for j in (range(len(invocations[i]))):
        if utils.dump_exist(args.region[i], invocations[i][j]):
          shutil.rmtree(os.path.join(var.CERE_DUMPS_PATH, args.region, str(invocation[i][j])), ignore_errors=True)

  # Now that everything is OK, launch compilation
  logger.info("Compiling capture mode for region {0} invocation {1}".format(args.region, args.invocation))
  try:
    logger.debug(subprocess.check_output("{0} && {1} CERE_MODE=\"dump --region={2} --invocation={3}\"".format(cere_configure.cere_config["clean_cmd"],
                                                                                                              cere_configure.cere_config["build_cmd"], args.region, args.invocation), stderr=subprocess.STDOUT, shell=True))
  except subprocess.CalledProcessError as err:
    logger.error(str(err))
    logger.error(err.output)
    logger.error("Compiling capture mode for region(s) {0} &  invocation(s) {1} failed".format(args.region, args.invocation))
    utils.mark_invalid(args.region, cere_error.EDUMP)
    return False
  if not args.norun:
    logger.info("Capturing invocation(s) {1} for region(s) {0}".format(args.region, args.invocation))
    try:
      logger.info(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
      #even if the capture run fails, maybe the region is dumped.
      logger.error(str(err))
      logger.error(err.output)

    # Checking if capture was
    has_failed_capture = False
    if not os.path.isdir("{0}/{1}/{2}".format(var.CERE_DUMPS_PATH, args.region, invocation)):
      logger.error("Capture failed for region(s) {0} invocation(s) {1}".format(args.region, args.invocation))
      utils.mark_invalid(args.region, cere_error.EDUMP)

    else:
      logger.info("Invocation(s) {1} succesfully captured for region(s) {0} ".format(args.region, args.invocation))

  return not has_failed_capture
