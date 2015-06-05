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

import sys
import argparse

import cere_configure
import cere_profile
import cere_dump
import cere_replay
import cere_test
import cere_select_max_cov
import cere_select_ilp
import cere_instrument
import cere_trace
import cere_regions
import cere_sanity_check
import cere_report
import cere_io_checker
import cere_selectinv

import logging
logger = logging.getLogger('CERE')

cere_plugins = {}

def start_log():
    logging.basicConfig(format='%(levelname)s %(asctime)s %(name)s : %(message)s',
                        datefmt='%m/%d/%Y %H:%M:%S',
                        level=logging.DEBUG,
                        filename='cere.log',
                        filemode='w')
    # define a Handler which writes INFO messages or higher to the sys.stderr
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    # set the same format for the console logger
    formatter = logging.Formatter('%(levelname)s %(asctime)s %(name)s : %(message)s', datefmt='%m/%d/%Y %H:%M:%S')
    # tell the handler to use this format
    console.setFormatter(formatter)
    # add the handler to the root logger
    logging.getLogger('').addHandler(console)
    logger.info("Start")

parser = argparse.ArgumentParser(description="CERE command line")
subparsers = parser.add_subparsers(help="Call CERE modules", dest="mode")

start_log()

cere_configure.init_module(subparsers, cere_plugins)
cere_profile.init_module(subparsers, cere_plugins)
cere_dump.init_module(subparsers, cere_plugins)
cere_replay.init_module(subparsers, cere_plugins)
cere_test.init_module(subparsers, cere_plugins)
cere_select_max_cov.init_module(subparsers, cere_plugins)
cere_select_ilp.init_module(subparsers, cere_plugins)
cere_instrument.init_module(subparsers, cere_plugins)
cere_trace.init_module(subparsers, cere_plugins)
cere_sanity_check.init_module(subparsers, cere_plugins)
cere_regions.init_module(subparsers, cere_plugins)
cere_report.init_module(subparsers, cere_plugins)
cere_io_checker.init_module(subparsers, cere_plugins)
cere_selectinv.init_module(subparsers, cere_plugins)

if __name__ == "__main__":
    args = parser.parse_args()
    status = 0

    if not cere_plugins[args.mode](args):
        status = 1

    logger.info("Stop")
    logging.shutdown()
    sys.exit(status)
