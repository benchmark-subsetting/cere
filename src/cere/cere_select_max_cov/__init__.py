#!/usr/bin/env python
# This file is part of CERE.
#
# Copyright (c) 2013-2015, Universite de Versailles St-Quentin-en-Yvelines
#
# CERE is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Foobar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

import argparse
import logging
import cere_configure
import os
import common.variables as var
from update_graph import update
from regions_selector import *

logger = logging.getLogger('Max-Cov selector')

def init_module(subparsers, cere_plugins):
    cere_plugins["select-max-cov"] = run
    profile_parser = subparsers.add_parser("select-max-cov", help="Select regions to maximise the matching coverage")
    profile_parser.add_argument("--max_error", default=15.0, help="Maximum tolerated error between invivo and invitro regions (Default: 15%)")
    profile_parser.add_argument("--min_coverage", type=float, default=0.1, help="Minimum percentage of region execution time (Default: 0.1%)")
    profile_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will overwrite any previous CERE measures (Default: False)")

def check_arguments(args):
    return True

def check_dependancies(args):
    #Check if the profiling file is present
    profile_file = "{0}/app.prof".format(var.CERE_PROFILE_PATH)
    graph = "{0}/graph_original.pkl".format(var.CERE_PROFILE_PATH)
    if not os.path.isfile(profile_file) or not os.path.isfile(graph):
        logger.critical('No profiling file or graph not created. Run: cere profile')
        return False
    return True

def run(args):
    if not cere_configure.init():
        return False
    if not check_dependancies(args):
        return False
    if not check_arguments(args):
        return False
    #Find matching codelets
    if not update(args):
        return False
    #Select matching codelets with best coverage
    if not solve_with_best_granularity(args):
        return False
    return True
