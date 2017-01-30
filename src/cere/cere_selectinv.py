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

from __future__ import print_function
import os
import logging
import shutil
import subprocess
import vars as var
import utils
import cere_configure
import csv
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from sklearn import cluster
from sklearn import preprocessing

logger = logging.getLogger('selectinv')
MAX_POINTS=10000

PALETTE = ["red","blue","green","yellow","black","grey","pink",
            "maroon","orange","purple","magenta","silver","golden",
            "brown","cyan"]

def init_module(subparsers, cere_plugins):
    cere_plugins["selectinv"] = run
    selectinv = subparsers.add_parser("selectinv", help="select representatives invocations from region trace")
    selectinv.add_argument('--region', required=True, help="region to select representatives")
    selectinv.add_argument('--force', '-f', action='store_true', help="Will force the CERE invocations selection")

def parse_codelet_csv(csvfile, codelet):
    with open(csvfile) as f:
        reader = csv.DictReader(f)
        try:
            entry = reader.next()
            assert(entry['Codelet Name'] == codelet)
            return entry
        except StopIteration:
            print("could not parse {csv} file".format(csv),
                  file=sys.stderr)
            sys.exit(1)

def parse_trace_file(tracefile):
    trace = np.fromfile(tracefile, dtype='d', count=-1)

    # filter the trace:
    # 1) odd items are cycles
    # 2) even items are invocation numbers
    size = len(trace) / 2
    return dict (cycles = trace[::2], invocations = trace[1::2],
                 size = size)

def subsample(trace, n):
    samples = np.random.choice(trace['size'], n)
    trace['invocations'] = trace['invocations'][samples,]
    trace['cycles'] = trace['cycles'][samples,]
    trace['size'] = n

def clusterize(trace):
    assert(len(trace['cycles']) == trace['size'])
    cycles = np.reshape(trace['cycles'], (trace['size'],1))

    # If the variation is negligeable, a clustering is useless
    if (np.max(cycles)-np.min(cycles))/np.max(cycles) < 0.10:
        return np.ones(trace['size'])

    # Normalize the distribution
    cycles = preprocessing.scale(cycles)

    min_samples = max(1,trace['size']/100)
    clusterer = cluster.DBSCAN(min_samples=min_samples, eps=.3)
    clusterer.fit(cycles)
    return clusterer.labels_

def clusterize_invocations(codelet, csvfile, tracefile):
    codelet_info = parse_codelet_csv(csvfile, codelet)
    in_vivo_cycles = float(codelet_info['CPU_CLK_UNHALTED_CORE'])

    trace = parse_trace_file(tracefile)

    # subsample trace if needed
    if trace['size'] > MAX_POINTS:
        subsample(trace, MAX_POINTS)

    # label the data using DBSCAN clusterer
    labels = clusterize(trace)

    # take all clusters except 'noise' cluster
    clusters = [ c for c in np.sort(np.unique(labels)) if c >= 0 ]

    # compute total weight for non-noise points
    total_weight = np.sum(trace['cycles'][labels >= 0])

    representatives = []
    weights = []
    repcycles = []

    # find representatives
    for c in clusters:
        inside_cluster = (labels == c)
        points = trace['cycles'][inside_cluster]
        invocation = trace['invocations'][inside_cluster]
        center = np.median(points)

        # representative is the closest invocation to the center
        repind = np.argmin(abs(points - center))
        rep = int(invocation[repind])
        representatives.append(rep)

        # cycles of the representative
        repcycles.append(points[repind])

        # weight of the cluster
        wei = (np.sum(points) / points[repind] *
               in_vivo_cycles  / total_weight)
        weights.append(wei)


    with file(codelet + '.invocations', 'w') as output:
        for i in range(len(clusters)):
            line = "{invocation} {weight} {cycles}\n".format(
                invocation = representatives[i],
                weight = weights[i],
                cycles = repcycles[i])
            output.write(line)

    #plot clusters
    f, ax = plt.subplots(1, figsize=(10, 4.16))
    for c, color in zip(clusters, PALETTE):
        inside_cluster = (labels == c)
        points = trace['cycles'][inside_cluster]
        invocations = trace['invocations'][inside_cluster]

        ax.plot(invocations, points, 'o', markerfacecolor=color,
                 markeredgecolor=color, markersize=4)

    ax.plot(trace['invocations'][labels < 0],
             trace['cycles'][labels < 0],
             'x', markerfacecolor='lightgray',
             markeredgecolor='lightgray', markersize=4)


    ax.grid('on')
    ax.set_ylabel('cycles')
    ax.set_xlabel('invocation')
    f.savefig(os.path.join(var.CERE_PLOTS_PATH, codelet + '_byPhase.png'),
            bbox_inches='tight', dpi=100)

def run(args):
    if not cere_configure.init():
        return False

    invocation_file = os.path.join(var.CERE_TRACES_PATH, args.region+".invocations")

    if os.path.isfile(invocation_file) and not args.force:
        logger.info("Invocations already selected for {0}. If you want to reselect use -f".format(args.region, invocation_file))
        return True
    if not utils.trace_exists(args.region):
        logger.error("No trace for {0}.\n\
                     Please first run cere trace --region={0}".format(args.region))
        return False
    bin_file = os.path.join(var.CERE_TRACES_PATH, args.region+".bin")
    csv_file = os.path.join(var.CERE_TRACES_PATH, args.region+".csv")

    clusterize_invocations(args.region, csv_file, bin_file)
    try:
        shutil.move("{0}.invocations".format(args.region), "{0}/{1}.invocations".format(var.CERE_TRACES_PATH, args.region))
    except IOError as err:
        logger.error(str(err))
        return False
    return True
