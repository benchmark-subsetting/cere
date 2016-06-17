#!/usr/bin/env python
import sys
import networkx as nx
sys.path.insert(0, "../../src/cere/")
from create_graph import remove_cycles, fix_self_coverage
import cere_configure
from graph_utils import plot, save_graph

if __name__ == "__main__":
    cere_configure.init()
    digraph = nx.DiGraph()
    nodes = [1,2,3,4,5,6,7]
    samples = 100
    digraph.add_nodes_from(nodes)
    for i in nodes:
        digraph.node[i]['_name'] = "Node_" + str(i)
        digraph.node[i]['_matching'] = True
        digraph.node[i]['_valid'] = True
        digraph.node[i]['_to_test'] = False
        digraph.node[i]['_small'] = False
        digraph.node[i]['_tested'] = False

    #Assign coverage and self_coverage
    digraph.node[1]['_self_coverage'] = 10.
    digraph.node[1]['_coverage'] = 60.
    digraph.node[2]['_self_coverage'] = 5.
    digraph.node[2]['_coverage'] = 40.
    digraph.node[3]['_self_coverage'] = 30.
    digraph.node[3]['_coverage'] = 13.
    digraph.node[4]['_self_coverage'] = 10.
    digraph.node[4]['_coverage'] = 0.
    digraph.node[5]['_self_coverage'] = 10.
    digraph.node[5]['_coverage'] = 0.
    digraph.node[6]['_self_coverage'] = 26.
    digraph.node[6]['_coverage'] = 26.
    digraph.node[7]['_self_coverage'] = 9.
    digraph.node[7]['_coverage'] = 9.

    #add edges
    digraph.add_edge(1, 3, weight = 30.)
    digraph.add_edge(1, 4, weight = 20.)
    digraph.add_edge(2, 3, weight = 35.)
    digraph.add_edge(3, 4, weight = 1.)
    digraph.add_edge(4, 5, weight = 1.)
    digraph.add_edge(5, 3, weight = 1.)
    digraph.add_edge(5, 6, weight = 4.)
    digraph.add_edge(5, 7, weight = 4.)
    digraph.add_edge(3, 6, weight = 22.)
    digraph.add_edge(4, 7, weight = 5.)

    plot(digraph, "debug")
    digraph = remove_cycles(digraph, samples)

    cycles = list(nx.simple_cycles(digraph))
    #If there is still a cycle we have a problem
    if len(cycles) != 0:
        print("Cycles are still present")
        plot(digraph, "final_fail")
        exit(1)
    if not fix_self_coverage(digraph, samples):
        print("Updating coverage failed")
        plot(digraph, "final_fail")
        exit(1)
    #If updating coverages failed to generate good coverage
    if digraph.node[3]["_self_coverage"] != 50 or digraph.node[3]["_coverage"] != 85:
        print("Error in removing cycle")
        plot(digraph, "final_fail")
        exit(1)
    in_degree = digraph.in_degree(3, weight='weight')
    out_degree = digraph.out_degree(3, weight='weight')
    if in_degree != digraph.node[3]["_coverage"] or in_degree - out_degree != digraph.node[3]["_self_coverage"]:
        print("Error in/out edges are wrong")
        plot(digraph, "final_fail")
        exit(1)
    plot(digraph, "final_success")
    exit(0)
