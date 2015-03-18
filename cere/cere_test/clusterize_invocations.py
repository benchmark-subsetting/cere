#!/usr/bin/env python
from __future__ import print_function
import csv
import numpy as np
from sklearn import cluster
from sklearn.preprocessing import StandardScaler

MAX_POINTS=50000

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
    cycles = np.reshape(trace['cycles'], (trace['size'],1))
    cycles = StandardScaler().fit_transform(cycles)
    clusterer = cluster.DBSCAN(min_samples=1)
    clusterer.fit(cycles)
    return clusterer.labels_

def clusterize_invocations(codelet, csvfile, tracefile):
    codelet_info = parse_codelet_csv(csvfile, codelet)
    in_vivo_cycles = float(codelet_info['CPU_CLK_UNHALTED_CORE'])

    trace = parse_trace_file(tracefile)

    # subsample trace if needed
    if trace['size'] > MAX_POINTS:
        subsample(trace, MAX_POINTS)

    # compute total weight
    total_weight = np.sum(trace['cycles'])

    # label the data using DBSCAN clusterer
    labels = clusterize(trace)

    clusters = np.sort(np.unique(labels))

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

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 4:
        print("usage: clusterize_invocations.py <codelet_name> <csv> <bin>",
              file=sys.stderr)
        sys.exit(1)

    clusterize_invocations(sys.argv[1], sys.argv[2], sys.argv[3])
