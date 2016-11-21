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

import argparse
import logging
import os
import subprocess
import shutil
import csv
import cere_configure
import cere_capture
import cere_replay
import cere_trace
import cere_selectinv
import vars as var
import utils
from graph_utils import *
import networkx as nx

logger = logging.getLogger('Check-matching')
ROOT = os.path.dirname(os.path.realpath(__file__))

def init_module(subparsers, cere_plugins):
    cere_plugins["check-matching"] = run
    matching_parser = subparsers.add_parser("check-matching", help="Test the matching for a list of region")
    matching_parser.add_argument('--region', help="Region to check matching")
    matching_parser.add_argument("--regions-file", help="The list of regions to check")
    matching_parser.add_argument("--max-error", default=15.0, help="Maximum tolerated error between invivo and invitro regions")
    matching_parser.add_argument('--force', '-f', action='store_true', help="Will force replay of regions.\
                                                                             Force is ignored for dumps and traces.")

def compute_error(n1, n2):
    if n1 == 0 and n2 == 0: return 100
    return (abs(n1-n2)/max(n1, n2))*100

def read_csv(File):
    try:
        FILE = open(File, 'rb')
    except (IOError):
        return []
    Dict = csv.DictReader(FILE, delimiter=',')
    return Dict

class Region():
    def __init__(self, r):
        self.region = r
        self.status = True
        self.invocation=1
        self.norun = False
        self.read = False
        self.regions_file = None
        self.noinstrumentation = False
        self.static = False
        self.plugin_instr=var.RDTSC_WRAPPER
        self.force = False
        self.invocations_data = []
        self.invitro_cycles=0.
        self.invivo_cycles=0.
        self.error = 0.
        self.coverage = 0.

    def compute_coverage(self):
        #There is two ways of computing coverage
        #1) If we have gperftool results:
        graph = load_graph()
        if graph:
            logger.info("Computing coverage using google perf tool")
            for n, d in graph.nodes(data=True):
                if d['_name'] == self.region:
                    self.coverage = float(d['_self_coverage'])
                    return
        #2) Compute the coverage manually
        elif os.path.isfile("{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH)):
            logger.info("Computing coverage using rdtsc tool")
            with open("{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH)) as app:
                reader = csv.DictReader(app)
                for row in reader:
                    app_cycles = float(row["CPU_CLK_UNHALTED_CORE"])
            self.coverage = (self.invivo_cycles/app_cycles)*100
        else:
            self.coverage = "NA"
            logger.warning("Cannot compute coverage for region {0}. Try to run cere profile".format(self.region))

    def measure_trace(self):
        res = cere_trace.run(self)
        if not res:
            self.invivo_cycles = 0
            return False
        with open("{0}/{1}.csv".format(var.CERE_TRACES_PATH, self.region)) as invivo:
            reader = csv.DictReader(invivo)
            for row in reader:
                self.invivo_cycles = float(row["CPU_CLK_UNHALTED_CORE"])
        return True

    def replay_invocations(self, force):
        if not os.path.isfile("{0}/{1}.invocations".format(var.CERE_TRACES_PATH, self.region)):
            logger.error("No invocation file for region {0}".format(self.region))
            return False
        err=False
        clust=0
        with open("{0}/{1}.invocations".format(var.CERE_TRACES_PATH, self.region)) as invocation_file:
            for line in invocation_file:
                self.force = False
                res=True
                clust = clust + 1
                infos = line.strip().split()
                self.invocation = infos[0]
                self.invitro_callcount = 10
                #Dump the required invocation
                cere_capture.run(self)
                #Replay it and compute error between invivo and invitro cycles for this invocation
                self.force = force
                if not cere_replay.run(self):
                    err=True
                    invitro_cycles = 0.
                else:
                    try:
                        if not os.path.isfile("{0}/{1}_{2}.csv".format(var.CERE_REPLAY_PATH, self.region, self.invocation)) or self.force:
                            shutil.move("{0}.csv".format(self.region), "{0}/{1}_{2}.csv".format(var.CERE_REPLAY_PATH, self.region, self.invocation))
                    except IOError as err:
                        logger.error(str(err))
                        logger.error("No results file. Maybe replay failed for {0} invocation {1}".format(self.region, self.invocation))
                        err=True
                        invitro_cycles = 0.
                    else:
                        with open("{0}/{1}_{2}.csv".format(var.CERE_REPLAY_PATH, self.region, self.invocation)) as invitro:
                            reader = csv.DictReader(invitro)
                            for row in reader:
                                invitro_cycles = float(row["CPU_CLK_UNHALTED_CORE"]) / float(row["Call Count"])
                matching_err = compute_error(invitro_cycles, float(infos[2]))
                #Aggregate invocations cycles, needed to compute invitro measure for the codelet
                self.invitro_cycles = self.invitro_cycles + invitro_cycles * float(infos[1])
                self.invocations_data.append([infos[0], clust, infos[1], invitro_cycles, infos[2], matching_err])
        return not err

    def check_region_matching(self):
        self.error = compute_error(self.invivo_cycles, self.invitro_cycles)

def dump_results(regions, max_error):
    with open("{0}/matching_error.csv".format(var.CERE_REPLAY_PATH), 'w') as regions_file, \
         open("{0}/invocations_error.csv".format(var.CERE_REPLAY_PATH), 'w') as invocations_file:
        regions_header = ["Codelet Name", "Invivo", "Invitro", "Error", "Exec Time"]
        invocations_header = ["Codelet Name", "Invocation", "Cluster", "Part", "Invitro", "Invivo", "Error"]
        regions_writer = csv.writer(regions_file)
        invocations_writer = csv.writer(invocations_file)
        regions_writer.writerow(regions_header)
        invocations_writer.writerow(invocations_header)
        for region in regions:
            logger.info("Results for region: {0}".format(region.region))
            if region.error > max_error:
                logger.info("  NOT MATCHING: In vitro = {0} & invivo = {1} (error = {2}%, coverage = {3}%)".format(region.invitro_cycles, region.invivo_cycles, region.error, region.coverage))
            else:
                logger.info("  MATCHING: In vitro = {0} & invivo = {1} (error = {2}%, coverage = {3}%)".format(region.invitro_cycles, region.invivo_cycles, region.error, region.coverage))
            regions_writer.writerow([region.region, region.invivo_cycles, region.invitro_cycles, region.error, region.coverage])
            for d in region.invocations_data:
                logger.info("      Invocation {0}: In vitro cycles = {1} & in vivo cycles = {2} (error = {3}%, part = {4})".format(d[0], d[3], d[4], d[5], d[2]))
                invocations_writer.writerow([region.region]+d)

def update_nodes(graph, max_allowed_error):
    lines = read_csv("{0}/matching_error.csv".format(var.CERE_REPLAY_PATH))
    for line in lines:
        #for region_name, error in matching.iteritems():
        #find the node in the graph
        for n,d in graph.nodes(data=True):
            if line["Codelet Name"] == d['_name']:
                d['_invivo'] = float(line["Invivo"])
                d['_invitro'] = float(line["Invitro"])
                d['_tested'] = True
                if float(line["Error"]) <= max_allowed_error:
                    d['_matching'] = True
                else:
                    d['_valid'] = False
                d['_error'] = float(line["Error"])
                if utils.is_invalid(d['_name']):
                    d['_error_message'] = utils.get_error_message(d['_name'])
                invocations = read_csv("{0}/invocations_error.csv".format(var.CERE_REPLAY_PATH))
                del d['_invocations'][:]
                for inv in invocations:
                    if inv["Codelet Name"] == d['_name']:
                        d['_invocations'].append({"Cluster":inv["Cluster"], "Invocation":inv["Invocation"],
                          "Part":round(float(inv["Part"]), 2), "Invivo (cycles)":"{:e}".format(float(inv["Invivo"])),
                          "Invitro (cycles)":"{:e}".format(float(inv["Invitro"])), "Error (%)":round(float(inv["Error"]), 2)})
                d['_tested'] = True
                d['_to_test'] = False
    save_graph(graph)
    plot(graph)

def run(args):
    if not cere_configure.init():
        return False

    if not (args.region or args.regions_file):
        logger.error("No region specified, use at least one of the following: --region, --regions-file")
        return False

    if (args.regions_file and args.region):
        logger.error("--region and --regions-file are exclusive")
        return False

    if args.region:
      regions = [args.region]
    elif args.regions_file:
      if not os.path.isfile(args.regions_file):
        logger.critical("\"{0}\" No such file.".format(args.regions_file))
        return False
      with open(args.regions_file, 'r') as regions_file:
        regions = [region.strip() for region in regions_file]

    err=False
    allRegions = []
    #For each region
    for r in regions:
        allRegions.append(Region(r))
        #first we need the trace
        res = allRegions[-1].measure_trace()
        if not res: err=True

        #Compute the coverage of this region
        allRegions[-1].compute_coverage()
        #We can clusterize invocations in performance classes
        res = cere_selectinv.run(allRegions[-1])
        if not res: err=True

    for idx, region in enumerate(allRegions):
        #Replay representative invocations
        res = allRegions[idx].replay_invocations(args.force)
        if not res: err=True

        #Compute error between invivo and in vitro
        allRegions[idx].check_region_matching()

    dump_results(allRegions, args.max_error)
    graph = load_graph()
    if graph != None:
        update_nodes(graph, args.max_error)
    return not err
