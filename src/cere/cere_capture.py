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
import utils
import vars as var
import errors as cere_error
import cere_configure

logger = logging.getLogger('Capture')

def init_module(subparsers, cere_plugins):
    cere_plugins["capture"] = run
    capture_parser = subparsers.add_parser("capture", help="captures a region")
    capture_parser.add_argument('--region', required=True, help="region to capture")
    capture_parser.add_argument('--invocation', type=int, help="invocation to capture (default 1)")
    capture_parser.add_argument('--norun', action='store_true', help="builds the capture-instrumented binary without running it")
    capture_parser.add_argument('--force', '-f', action='store_true', help="overwrites previous existing dump (default False)")

def find_invocations(chosen_invoc, region):
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
  return tmp_invocations

def run(args):
  if not cere_configure.init():
    return False

  invocations = find_invocations(args.invocation, args.region)
  if not invocations:
    return False
  for invocation in invocations:
    if not args.force:
      if utils.dump_exist(args.region, invocation):
        logger.info("Dump already exists for region {0} invocation {1}".format(args.region, invocation))
        return True
      if utils.is_invalid(args.region):
        logger.warning("{0} is invalid. Skipping capture".format(args.region))
        return False
    else:
      shutil.rmtree(os.path.join(var.CERE_DUMPS_PATH, args.region, str(invocation)), ignore_errors=True)

    logger.info("Compiling capture mode for region {0} invocation {1}".format(args.region, invocation))
    try:
      logger.debug(subprocess.check_output("{0} && {1} CERE_MODE=\"dump --region={2} --invocation={3}\"".format(cere_configure.cere_config["clean_cmd"],
                                          cere_configure.cere_config["build_cmd"], args.region, invocation), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
      logger.error(str(err))
      logger.error(err.output)
      logger.error("Compiling capture mode for region {0} invocation {1} failed".format(args.region, invocation))
      utils.mark_invalid(args.region, cere_error.EDUMP)
      return False
    if not args.norun:
      logger.info("Capturing invocation {1} for region {0}".format(args.region, invocation))
      try:
        logger.info(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
      except subprocess.CalledProcessError as err:
        #even if the capture run fails, maybe the region is dumped.
        logger.error(str(err))
        logger.error(err.output)
      if not os.path.isdir("{0}/{1}/{2}".format(var.CERE_DUMPS_PATH, args.region, invocation)):
        logger.error("Capture failed for region {0} invocation {1}".format(args.region, invocation))
        utils.mark_invalid(args.region, cere_error.EDUMP)
        return False
      else:
        logger.info("Invocation {1} succesfully captured for region {0} ".format(args.region, invocation))
  return True
