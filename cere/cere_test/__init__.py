#!/usr/bin/env python

import argparse
import logging
import os
import shutil
import csv
import cere_configure
import cere_dump
import cere_replay

ROOT = os.path.dirname(os.path.realpath(__file__))

def init_module(subparsers, cere_plugins):
    cere_plugins["test"] = run
    profile_parser = subparsers.add_parser("test", help="Test the matching for a list of region")
    profile_parser.add_argument("--regions", help="The list of codelets to test")
    profile_parser.add_argument("--max_error", default=15.0, help="Maximum tolerated error between invivo and invitro regions")
    profile_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will overwrite any previous CERE measures")

def compute_error(n1, n2):
    return (abs(n1-n2)/max(n1, n2))*100

class Region():
    def __init__(self, r, f):
        self.region = r
        self.status = True
        self.invocation=1
        self.norun = False
        self.noinstrumentation = False
        self.force = f
        self.invocations_data = []
        self.invitro_cycles=0.
        self.invivo_cycles=0.
        self.error = 0.
        self.coverage = 0.

    def measure_trace(self):
        if not os.path.isfile("{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region))\
        or not os.path.isfile("{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], self.region))\
        or self.force:
            #Measure invivo trace
            ret = os.system("{0} MODE=\"original --instrument --region={1} --trace\" -B".format(cere_configure.cere_config["build_cmd"], self.region))
            if ret:
                logging.critical("{0} MODE=\"original --instrument --region={1} --trace\": Failed".format(cere_configure.cere_config["build_cmd"], self.region))
                return False
            os.system(cere_configure.cere_config["run_cmd"])
            try:
                shutil.move("{0}.bin".format(self.region), "{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], self.region))
                shutil.move("rdtsc_result.csv", "{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region))
            except IOError as err:
                logging.critical(str(err))
                return False
        else:
            logging.info("Using previous trace results")

        with open("{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region)) as invivo:
            reader = csv.DictReader(invivo)
            for row in reader:
                self.invivo_cycles = float(row["CPU_CLK_UNHALTED_CORE"])
        return True

    def clusterize_invocations(self):
        logging.info("Computing clustering info")
        if not os.path.isfile("{0}/{1}.invocation".format(cere_configure.cere_config["cere_measures_path"], self.region)) or self.force:
            os.system("{0}/clusterize_invocations.R {2} {1}/{2}.csv {1}/{2}.bin".format(ROOT, cere_configure.cere_config["cere_measures_path"], self.region))
            if not os.path.isfile("{0}.invocations".format(self.region)):
                logging.critical("Error for {0}: No clustering infos".format(self.region))
                return False
            else:
                try:
                    shutil.move("{0}.invocations".format(self.region), "{0}/{1}.invocation".format(cere_configure.cere_config["cere_measures_path"], self.region))
                except IOError as err:
                    logging.critical(str(err))
                    return False
        else:
            logging.info("Using previous clustering infos")
        return True

    def replay_invocations(self):
        if not os.path.isfile("{0}/{1}.invocation".format(cere_configure.cere_config["cere_measures_path"], self.region)):
            logging.critical("No invocation file for region {0}".format(self.region))
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
                res = cere_replay.run(self)
                try:
                    shutil.move("rdtsc_result.csv", "{0}/{1}_{2}.csv".format(cere_configure.cere_config["cere_measures_path"], self.region, self.invocation))
                except IOError as err:
                    logging.critical(str(err))
                    err=True
                    res = False
                #Compute error between invivo and invitro cycles for this invocation
                if not res:
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
            logging.info(region.region)
            if region.error > max_error:
                logging.info("  NOT MATCHING: In vitro = {0} & invivo = {1} (error = {2}%)".format(region.invitro_cycles, region.invivo_cycles, region.error))
            else:
                logging.info("  MATCHING: In vitro = {0} & invivo = {1} (error = {2}%)".format(region.invitro_cycles, region.invivo_cycles, region.error))
            regions_writer.writerow([region.region, region.invivo_cycles, region.invitro_cycles, region.error, region.coverage])
            for d in region.invocations_data:
                logging.info("      Invocation {0}: In vitro cycles = {1} & in vivo cycles = {2} (error = {3}%)".format(d[0], d[3], d[4], d[5]))
                invocations_writer.writerow([region.region]+d)

def run(args):
    cere_configure.init()
    if not os.path.isfile(args.regions):
        if not os.path.isfile("{0}/selected_regions".format(cere_configure.cere_config["cere_measures_path"])):
            logging.critical("The default region file is not present:\n    Choose a file manually\n    Run cere filter or cere regions")
            return False
        else: region_file = "{0}/selected_regions".format(cere_configure.cere_config["cere_measures_path"])
    else: region_file = args.regions
    with open(region_file, 'r') as region_file:
        regions = [region.strip() for region in region_file]
    err=False
    allRegions = []
    #For each region
    for r in regions:
        region = Region(r, args.force)
        #first we need the trace
        res = region.measure_trace()
        if not res: 
            err=True
            continue
        #We can clusterize invocations in performance classes
        res = region.clusterize_invocations()
        if not res:
            err=True
            continue
        #Replay representative invocations
        res = region.replay_invocations()
        if not res: err=True
        #Compute error between invivo and in vitro
        res = region.check_region_matching()
        allRegions.append(region)

    dump_results(allRegions, args.max_error)
    return not err
