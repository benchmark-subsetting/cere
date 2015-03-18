#!/usr/bin/env python
from __future__ import print_function
import csv
import numpy as np
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
from sklearn import cluster
from sklearn.preprocessing import StandardScaler


MAX_POINTS=50000

PALETTE = ["red","blue","green","yellow","black","grey","pink",
            "maroon","orange","purple","magenta","silver","golden",
            "brown","cyan"]

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

def clusterize(trace):
    min_samples = max(1,trace['size']/1000)
    cycles = np.reshape(trace['cycles'], (trace['size'],1))
    cycles = StandardScaler().fit_transform(cycles)
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
    f.savefig('cere_measures/plots/' + codelet + '_byPhase.png',
            bbox_inches='tight', dpi=100)


if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        print("usage: clusterize_invocations.py <codelet_name> <csv> <bin>",
              file=sys.stderr)
        sys.exit(1)

    clusterize_invocations(sys.argv[1], sys.argv[2], sys.argv[3])
