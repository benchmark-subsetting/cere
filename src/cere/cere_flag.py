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
import csv
import argparse
import logging
import subprocess
import cere_configure
import cere_replay
import vars as var
import graph_utils

logger = logging.getLogger('Flag')

def init_module(subparsers, cere_plugins):
  cere_plugins["flag"] = run
  flag_parser = subparsers.add_parser("flag", help="test flags performance")
  flag_parser.add_argument('--region', help="region to replay")
  flag_parser.add_argument('FLAGSFILE', type=file, help="file of flags to test")
  flag_parser.add_argument('--invitro-callcount', type=int, default=10, help="Meta-repetitions for the replay (Default 10)")
  flag_parser.add_argument('--force', '-f', action='store_true', help="force to replay (Delete previous measure)")

def read_csv(File):
  Dict = csv.DictReader(File, delimiter=',')
  return Dict

def clean_environ():
  if "CERE_BACKEND_FLAGS" in os.environ:
    del os.environ["CERE_BACKEND_FLAGS"]
  if "CERE_MIDEND_FLAGS" in os.environ:
    del os.environ["CERE_MIDEND_FLAGS"]
  del os.environ["CERE_LLC"]

def get_predicted_cycles(region):
  result_file = open("{0}/{1}".format(var.CERE_REPLAY_PATH, region), 'r')
  predited_cycles = result_file.readline()
  return float(predited_cycles)

def set_env(line):
  if "mid_end" in line:
    os.environ["CERE_MIDEND_FLAGS"]=line["mid_end"]
  if "back_end" in line:
    os.environ["CERE_BACKEND_FLAGS"]=line["back_end"]

def dump_results(best_flags, flags_file, flags):
  with open("{0}/regions_flags.csv".format(var.CERE_FLAGS_PATH), 'w') as regions_file:
    regions_header = ["Region", "Id", "Mid_end", "Back_end", "Cycles", "Original_cycles"]
    regions_writer = csv.writer(regions_file)
    regions_writer.writerow(regions_header)
    for region in best_flags:
      if int(region["best_id"]) < 0:
        regions_writer.writerow([region["region"], -1, "default", "default", region["best_cycles"], region["original_cycles"]])
      else:
        for x in range(0, int(region["best_id"])-1):
          flags.next()
        for line in flags:
          regions_writer.writerow([region["region"], region["best_id"], line["mid_end"], line["back_end"], region["best_cycles"], region["original_cycles"]])
          break
        flags_file.seek(0)
        flags.next()
  logger.info("Results dumped in {0}/regions_flags.csv".format(var.CERE_FLAGS_PATH))

def run_replay(flags, args, history):
  best_id = -1
  best_cycles = float("inf")

  #Open result file
  with open("{0}/{1}.csv".format(var.CERE_FLAGS_PATH, args.region), 'a') as region_file:
    region_writer = csv.writer(region_file)
    for line in flags:
      #Replay if "id" not already measured
      if line["id"] not in history:
        logger.info("Running ID: {0}".format(line["id"]))
        set_env(line)

        #Set attributes for the replay pass.
        args.static=False
        args.force=True
        args.invocation=None
        args.noinstrumentation=False
        args.norun=False
        args.plugin_instr=var.RDTSC_WRAPPER

        if cere_replay.run(args):
          predicted_cycles = get_predicted_cycles(args.region)
          region_writer.writerow([line["id"], line["mid_end"], line["back_end"], predicted_cycles])
          region_file.flush()
        else:
          predicted_cycles = float("inf")
      #Get previous measure for this sequence of flags
      else:
        predicted_cycles = float(history[line["id"]]["cycles"])

      if predicted_cycles < best_cycles:
        best_cycles = predicted_cycles
        best_id = line["id"]

  args.FLAGSFILE.seek(0)
  flags.next()

  return best_id, best_cycles

def compute_theorical_speedup(best_flags, graph):
  logger.info("Computing theorical speedup with best flags")
  if not graph:
    for region in best_flags:
      if int(region["best_id"]) < 0:
        logger.info("{0}: Can't find a better sequence than the default compilation flags.".format(region["region"]))
        continue
      region_speedup = round(abs(float(float(region["best_cycles"]) / float(region["original_cycles"]) - 1)) * 100, 2)
      logger.info("{3}: Default cycles = {0}. Best id = {1}. Best cycles = {2}. Speedup = {4}%".format(region["original_cycles"], region["best_id"], region["best_cycles"], region["region"], region_speedup))
  else:
    total_hybrid_speedup = 0
    for region in best_flags:
      if int(region["best_id"]) < 0:
        logger.info("{0}: Can't find a better sequence than the default compilation flags.".format(region["region"]))
        continue
      for n, d in graph.nodes(data=True):
        if region["region"] == d['_name']:
          region_speedup = round(abs(float(float(region["best_cycles"]) / float(d['_invivo']) - 1)) * 100, 2)
          logger.info("{2}: Coverage = {4}. Default cycles = {0}. Best id = {5}. Best cycles = {1}. Speedup = {3}%".format(d['_invivo'], region["best_cycles"], region["region"], region_speedup, d['_coverage'], region["best_id"]))
          total_hybrid_speedup = total_hybrid_speedup + (region_speedup * d['_coverage']) / 100
    logger.info("Expected hybrid speedup = {0}%".format(round(total_hybrid_speedup,2)))

def get_best_flags(region, best_history, original_cycles, best_id, best_cycles):
  if region in best_history:
    cycles = best_history[region]["best_cycles"]
    id_ = best_history[region]["best_id"]
  else:
    cycles = original_cycles
    id_ = -1

  if float(best_cycles) < float(cycles):
    best_flags = {"region":region, "best_id":best_id, "best_cycles":best_cycles, "original_cycles":cycles}
  else:
    best_flags = {"region":region, "best_id":id_, "best_cycles":cycles, "original_cycles":cycles}
  return best_flags

def get_region_history(region, force):
  flags_history = {}
  if os.path.isfile("{0}/{1}.csv".format(var.CERE_FLAGS_PATH, region)) and not force:
    region_flags = read_csv(open("{0}/{1}.csv".format(var.CERE_FLAGS_PATH, region), 'rb'))
    for row in region_flags:
      flags_history[row["id"]] = {"mid_end":row["mid_end"], "back_end":row["back_end"], "cycles":row["cycles"]}
  else:
    with open("{0}/{1}.csv".format(var.CERE_FLAGS_PATH, region), 'w') as region_file:
      region_header = ["id", "mid_end", "back_end", "cycles"]
      region_writer = csv.writer(region_file)
      region_writer.writerow(region_header)
  return flags_history

def run(args):
  if not cere_configure.init():
    return False
  #Need to save this because args.force will be changed by the replay pass
  force_flags = args.force
  #Array to store best flags for every regions
  best_flags = []
  
  graph = graph_utils.load_graph()
  if graph == None:
    logger.error("CERE graph is missing. Please run cere profile")
    return False

  #Load previous best results
  best_history = {}
  if os.path.isfile("{0}/regions_flags.csv".format(var.CERE_FLAGS_PATH)) and not force_flags:
    flags_file = read_csv(open("{0}/regions_flags.csv".format(var.CERE_FLAGS_PATH), 'rb'))
    for row in flags_file:
      best_history[row["Region"]] = {"best_id":row["Id"], "best_cycles":row["Cycles"], "original_cycles":row["Original_cycles"]}

  #Use llc as backend
  os.environ["CERE_LLC"]="llc"
  #Read flags file
  flags = read_csv(args.FLAGSFILE)

  #Run the requested region
  if args.region:
    found = False
    #Load region information
    for n, d in graph.nodes(data=True):
      if d['_name'] == args.region:
        found = True
        break
    if not found:
      logger.error("{0} does not exist. Valid region name can be found in {1}`".format(args.region, cere_configure.cere_config["regions_infos"]))
      return False
    if not d['_tested']:
      logger.error("{0} not tested. Please run `cere check-matching --region={0}`".format(args.region))
      return False
    if not d['_matching']:
      logger.warning("{0} replay is marked as not matching. Results might not be accurate.".format(args.region))

    #Load any previous measures for this region
    flags_history = get_region_history(args.region, force_flags)

    #Replay region with flags to test
    best_id, best_cycles = run_replay(flags, args, flags_history)

    best_flags.append(get_best_flags(args.region, best_history, d['_invivo'], best_id, best_cycles))

  #or run selected regions from ilp selector
  elif not args.region:
    graph = graph_utils.load_graph()
    if graph == None or graph.graph['selector'] != "ILP":
      logger.error("Selected regions missing. First run cere select-ilp or choose a region manually with cere flag --region.")
      return False
    #Run selected regions
    for n, d in graph.nodes(data=True):
      if d["_selected"]:
        args.region=d['_name']

        #Load any previous measures for this region
        flags_history = get_region_history(args.region, force_flags)

        #Replay region with flags to test
        best_id, best_cycles = run_replay(flags, args, flags_history)

        best_flags.append(get_best_flags(args.region, best_history, d['_invivo'], best_id, best_cycles))

  compute_theorical_speedup(best_flags, graph)
  dump_results(best_flags, args.FLAGSFILE, flags)
  clean_environ()
  return True
