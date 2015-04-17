#!/usr/bin/env python

import argparse
import logging
import os
import subprocess
import shutil
import csv
import cere_configure
import cere_dump
import cere_replay
import cere_trace
import common.variables as var
import common.utils as utils

logger = logging.getLogger('Test')
ROOT = os.path.dirname(os.path.realpath(__file__))

def init_module(subparsers, cere_plugins):
    cere_plugins["test"] = run
    profile_parser = subparsers.add_parser("test", help="Test the matching for a list of region")
    profile_parser.add_argument("--regions", help="The list of regions to test in a file")
    profile_parser.add_argument("--max_error", default=15.0, help="Maximum tolerated error between invivo and invitro regions")
    profile_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will overwrite any previous CERE measures")

def compute_error(n1, n2):
    if n1 == 0 and n2 == 0: return 100
    return (abs(n1-n2)/max(n1, n2))*100

class Region():
    def __init__(self, r, f):
        self.region = r
        self.status = True
        self.invocation=1
        self.norun = False
        self.read = False
        self.regions_file = None
        self.noinstrumentation = False
        self.wrapper=var.RDTSC_WRAPPER
        self.force = f
        self.invocations_data = []
        self.invitro_cycles=0.
        self.invivo_cycles=0.
        self.error = 0.
        self.coverage = 0.

    def compute_coverage(self):
        #There is two ways of computing coverage
        #1) If we have gperftool results:
        from common.graph_utils import load_graph
        import networkx as nx
        graph = load_graph()
        if graph:
            logger.info("Computing coverage using google perf tool")
            for n, d in graph.nodes(data=True):
                if d['_name'] == self.region.replace("invivo", "extracted"):
                    self.coverage = float(d['_self_coverage'])
                    return
        #2) Compute the coverage manually
        elif os.path.isfile("{0}/app_cycles.csv".format(cere_configure.cere_config["cere_measures_path"])):
            logger.info("Computing coverage using rdtsc tool")
            with open("{0}/app_cycles.csv".format(cere_configure.cere_config["cere_measures_path"])) as app:
                reader = csv.DictReader(app)
                for row in reader:
                    app_cycles = float(row["CPU_CLK_UNHALTED_CORE"])
            self.coverage = (self.invivo_cycles/app_cycles)*100
        else:
            self.coverage = "NA"
            logger.warning("Cannot compute coverage for region {0}. Try to run cere profile".format(self.region.replace("invivo", "extracted")))

    def measure_trace(self):
        res = cere_trace.run(self)
        if not res:
            self.invivo_cycles = 0
            return False
        with open("{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region)) as invivo:
            reader = csv.DictReader(invivo)
            for row in reader:
                self.invivo_cycles = float(row["CPU_CLK_UNHALTED_CORE"])
        return True

    def clusterize_invocations(self):
        logger.info("Computing clustering info")
        if not utils.trace_exists(self.region):
            logger.error("No trace, Cannot clusterize invocation for region {0}".format(self.region))
            return False
        if not os.path.isfile("{0}/{1}.invocation".format(cere_configure.cere_config["cere_measures_path"], self.region)) or self.force:
            try:
                logger.debug(subprocess.check_output("{0}/clusterize_invocations.py {2} {1}/{2}.csv {1}/{2}.bin".format(ROOT, cere_configure.cere_config["cere_measures_path"], self.region), stderr=subprocess.STDOUT, shell=True))
            except subprocess.CalledProcessError as err:
                logger.error(str(err))
                logger.error(err.output)
            if not os.path.isfile("{0}.invocations".format(self.region)):
                logger.critical("Error for {0}: No clustering infos".format(self.region))
                return False
            else:
                try:
                    shutil.move("{0}.invocations".format(self.region), "{0}/{1}.invocation".format(cere_configure.cere_config["cere_measures_path"], self.region))
                except IOError as err:
                    logger.error(str(err))
                    logger.error(err.output)
                    return False
        else:
            logger.info("Using previous clustering infos")
        return True

    def replay_invocations(self):
        if not os.path.isfile("{0}/{1}.invocation".format(cere_configure.cere_config["cere_measures_path"], self.region)):
            logger.error("No invocation file for region {0}".format(self.region))
            return False
        err=False
        clust=0
        with open("{0}/{1}.invocation".format(cere_configure.cere_config["cere_measures_path"], self.region)) as invocation_file:
            for line in invocation_file:
                res=True
                clust = clust + 1
                infos = line.strip().split()
                self.region = self.region.replace("invivo", "extracted")
                self.invocation = infos[0]
                self.invitro_callcount = 10
                res = cere_replay.run(self)
                #Compute error between invivo and invitro cycles for this invocation
                if not res:
                    err=True
                    invitro_cycles = 0.
                else:
                    try:
                        if not os.path.isfile("{0}/{1}_{2}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region, self.invocation)):
                            shutil.move("{0}.csv".format(self.region), "{0}/{1}_{2}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region, self.invocation))
                    except IOError as err:
                        logger.error(str(err))
                        logger.error(err.output)
                        logger.error("No results file. Maybe replay failed for {0} invocation {1}".format(self.region, self.invocation))
                        err=True
                        invitro_cycles = 0.
                    else:
                        with open("{0}/{1}_{2}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region, self.invocation)) as invitro:
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
    with open("{0}/matching_error.csv".format(cere_configure.cere_config["cere_measures_path"]), 'w') as regions_file, \
         open("{0}/invocations_error.csv".format(cere_configure.cere_config["cere_measures_path"]), 'w') as invocations_file:
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

def run(args):
    cere_configure.init()
    region_file = args.regions
    if not region_file:
        if not os.path.isfile("{0}/selected_regions".format(cere_configure.cere_config["cere_measures_path"])):
            logger.critical("The default region file is not present ({0}/selected_regions):\n    Choose a file manually with --regions=[file]\n    Run cere filter or cere regions".format(cere_configure.cere_config["cere_measures_path"]))
            return False
        else: region_file = "{0}/selected_regions".format(cere_configure.cere_config["cere_measures_path"])
    if not os.path.isfile(region_file):
        logger.critical("\"{0}\" No such file. Please check name or put the region name in a file".format(region_file))
        return False

    with open(region_file, 'r') as region_file:
        regions = [region.strip() for region in region_file]
    err=False
    allRegions = []
    #For each region
    for r in regions:
        region = Region(r, args.force)
        #first we need the trace
        res = region.measure_trace()
        if not res: err=True

        #Compute the coverage of this region
        region.compute_coverage()
        #We can clusterize invocations in performance classes
        res = region.clusterize_invocations()
        if not res: err=True

        #Replay representative invocations
        res = region.replay_invocations()
        if not res: err=True

        #Compute error between invivo and in vitro
        res = region.check_region_matching()
        allRegions.append(region)

    dump_results(allRegions, args.max_error)
    return not err
