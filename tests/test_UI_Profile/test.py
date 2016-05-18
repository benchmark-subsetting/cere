#!/usr/bin/env python

import sys
import os
import logging
import networkx as nx
sys.path.insert(0, "../../src/cere/")
import cere_configure
from graph_utils import load_graph
import vars as var

logger = logging.getLogger('Test Profile')

if __name__ == "__main__":
  cere_configure.init()

  #Check if call graph exists
  graph = load_graph()
  if graph == None:
    logger.critical("Cannot load graph.")
    sys.exit(1)

  #Is there any cere region?
  if( len(graph.nodes()) == 0):
    logger.info('Graph is empty.')
    sys.exit(1)

  #Check if measuring application cycles works
  if not os.path.isfile("{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH)):
    logger.critical('Measuring cycles failed.')
    sys.exit(1)

  logger.info("Profiling ok.")
  sys.exit(0)
