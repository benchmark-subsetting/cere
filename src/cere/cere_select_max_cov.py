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
import cere_configure
import os
import vars as var
from max_cov_update_graph import update
from regions_selector import *

logger = logging.getLogger('Max-Cov selector')

def init_module(subparsers, cere_plugins):
    cere_plugins["select-max-cov"] = run
    profile_parser = subparsers.add_parser("select-max-cov", help="Select regions to maximise the matching coverage")
    profile_parser.add_argument("--max-error", default=15.0, help="Maximum tolerated error between invivo and invitro regions")
    profile_parser.add_argument("--min-coverage", type=float, default=1.0, help="Minimum percentage of region execution time")
    profile_parser.add_argument('--force', '-f', action='store_true', help="Will overwrite any previous CERE measures")

def check_arguments(args):
    return True

def check_dependancies(args):
    #Check if the profiling file is present
    profile_file = "{0}/app.prof".format(var.CERE_PROFILE_PATH)
    graph = "{0}/graph_.pkl".format(var.CERE_PROFILE_PATH)
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
