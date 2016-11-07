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
import sys
import argparse
import logging
import shutil
import subprocess
import csv
import cere_configure
import cere_capture
import vars as var
import utils
import errors as cere_error

logger = logging.getLogger('Replay')

PREDICTION_MODE=False

def init_module(subparsers, cere_plugins):
  cere_plugins["replay"] = run
  replay_parser = subparsers.add_parser("replay", help="replay a region")
  replay_parser.add_argument('--region', required=True, help="Region to replay")
  replay_parser.add_argument('--invocation', type=int, help="invocation to replay")
  replay_parser.add_argument('--invitro-callcount', type=int, default=10, help="Meta-repetition for the replay (Default 10)")
  replay_parser.add_argument('--plugin-instr', default=var.RDTSC_WRAPPER, help="Plugin to instrument the replay")
  replay_parser.add_argument('--static', action='store_true', help="Produce a statically linked binary")
  replay_parser.add_argument('--noinstrumentation', action='store_true', help="Replay without instrumentation")
  replay_parser.add_argument('--norun', action='store_true', help="If you don't want to automatically run the replay")
  replay_parser.add_argument('--force', '-f', action='store_true', help="force to replay (Delete previous measure)")

def find_invocations(chosen_invoc, region):
  global PREDICTION_MODE
  tmp_invocations = {}
  if not chosen_invoc:
    #Find representatives invocations
    if not os.path.isfile("{0}/{1}.invocations".format(var.CERE_TRACES_PATH, region)):
      logger.error("Representative invocations file is missing.\n\
                        Please run cere selectinv --region={0}\n\
                        Or choose manually an invocation with --invocation".format(region))
      return None
    #Store invocations and the corresponding cluster part
    with open("{0}/{1}.invocations".format(var.CERE_TRACES_PATH, region)) as invocation_file:
      for line in invocation_file:
        infos = line.strip().split()
        tmp_invocations[infos[0]] = float(infos[1])
    PREDICTION_MODE=True
  else:
    tmp_invocations[chosen_invoc] = 1
  return tmp_invocations

def compute_predicted_time(region, invocations):
  #Use representatives invocations replay measures to predict
  #the region execution time.
  invitro_cycles = 0.
  err = False
  logger.info("Predicted cycles for region: {0}".format(region))
  for invocation, part in invocations.iteritems():
    replay_file = "{0}/{1}_{2}.csv".format(var.CERE_REPLAY_PATH, region, invocation)
    if os.path.isfile(replay_file):
      with open(replay_file) as invitro:
        reader = csv.DictReader(invitro)
        for row in reader:
          invocation_cycles = float(row["CPU_CLK_UNHALTED_CORE"]) / float(row["Call Count"])
      logger.info(" Invocation {0}: In vitro cycles = {1} (part = {2})".format(invocation, invocation_cycles, part))
      invitro_cycles = invitro_cycles + invocation_cycles * part
    else:
      logger.error("  Invocation {0}: No results file. Maybe replay failed".format(invocation))
      err=True
  if err:
    invitro_cycles = 0
  return invitro_cycles

def dump_result(region, predicted_cycles):
  result_file = open("{0}/{1}".format(var.CERE_REPLAY_PATH, region), 'w')
  result_file.write("{0}\n".format(predicted_cycles))
  result_file.close()

def run(args):
  if not cere_configure.init():
    return False
  if utils.is_invalid(args.region) and not args.force:
    logger.warning("{0} is invalid. Skipping replay".format(args.region))
    return False

  invocations = find_invocations(args.invocation, args.region)
  if not invocations:
    return False
  if (PREDICTION_MODE and args.plugin_instr != var.RDTSC_WRAPPER):
    logger.warning("You are not using the default library. Computing predicted time\n\
                    may not work if the replay output is not the same")
  for invocation, part in invocations.iteritems():
    if os.path.isfile("{0}/{1}_{2}.csv".format(var.CERE_REPLAY_PATH, args.region, invocation)) and not args.force:
      logger.warning("Replay already measured for {0} invocation {1}.".format(args.region, invocation))
      continue
    if not utils.dump_exist(args.region, invocation):
      logger.error("Memory dump is missing for {0} invocation {1}.\n\
                    Run cere capture --region={0} [--invocation={1}]".format(args.region, invocation))
      return False
    if args.noinstrumentation:
      instru_cmd = ""
      logger.info("Compiling replay mode for region {0} invocation {1} without instrumentation".format(args.region, invocation))
    else:
      instru_cmd = "--instrument"
      logger.info("Compiling replay mode for region {0} invocation {1} with instrumentation".format(args.region, invocation))

    if args.static:
      static_cmd = "--static"
      logger.info("Static mode enabled".format(args.region, invocation))
    else:
      static_cmd = ""

    try:
      logger.debug(
        subprocess.check_output("{0} && {1} CERE_REPLAY_REPETITIONS={6} CERE_MODE=\"replay --region={2} --invocation={3} {4} --wrapper={5} {7}\"".format(
        cere_configure.cere_config["clean_cmd"], cere_configure.cere_config["build_cmd"],
          args.region, invocation, instru_cmd, args.plugin_instr, args.invitro_callcount, static_cmd), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
      logger.error(str(err))
      logger.error(err.output)
      logger.error("Compiling replay mode for region {0} invocation {1} Failed".format(args.region, invocation))
      utils.mark_invalid(args.region, cere_error.EREPLAY)
      return False
    if not args.norun:
      logger.info("Replaying invocation {1} for region {0}".format(args.region, invocation))
      try:
        logger.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
      except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.error("Replay failed for {0} invocation {1}".format(args.region, invocation))
        utils.mark_invalid(args.region, cere_error.EREPLAY)
        return False
      #Save replay measures files
      if PREDICTION_MODE:
        try:
          shutil.move("{0}.csv".format(args.region), "{0}/{1}_{2}.csv".format(var.CERE_REPLAY_PATH, args.region, invocation))
        except IOError as err:
          logger.error(str(err))
          logger.error("  No results file. Maybe replay failed".format(invocation))
          return False
  if PREDICTION_MODE:
    predicted_cycles = compute_predicted_time(args.region, invocations)
    dump_result(args.region, predicted_cycles)
    logger.info(" Overall predicted cycles = {0}".format(predicted_cycles))
  return True
