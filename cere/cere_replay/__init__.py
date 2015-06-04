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

import os
import sys
import argparse
import logging
import shutil
import subprocess
import cere_configure
import cere_dump
import common.variables as var
import common.utils as utils
import common.errors as cere_error

logger = logging.getLogger('Replay')

def init_module(subparsers, cere_plugins):
    cere_plugins["replay"] = run
    replay_parser = subparsers.add_parser("replay", help="replay a region")
    replay_parser.add_argument('--region', required=True, help="Region to replay")
    replay_parser.add_argument('--invocation', type=int, default=1, help="invocation to replay (Default 1)")
    replay_parser.add_argument('--invitro-callcount', type=int, default=10, help="Meta-repetition for the replay (Default 10)")
    replay_parser.add_argument('--noinstrumentation', type=bool, const=True, default=False, nargs='?', help="=Replay without instrumentation")
    replay_parser.add_argument('--wrapper', default=var.RDTSC_WRAPPER, help="Wrapper used to make the link between cere interface and your library")
    replay_parser.add_argument('--norun', type=bool, default=False, help="=If you don't want to automatically run the replay")
    replay_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will re-dump any previous CERE dumps")

def run(args):
    if not cere_configure.init():
        return False
    if utils.is_invalid(args.region) and not args.force:
        logger.warning("{0} is invalid. Skipping replay".format(args.region))
        return False
    if os.path.isfile("{0}/{1}_{2}.csv".format(var.CERE_REPLAY_PATH, args.region, args.invocation)) and not args.force:
        logger.info("Keeping previous replay measures for {0} invocation {1}.".format(args.region, args.invocation))
        return True
    if args.noinstrumentation:
        instru_cmd = ""
        logger.info("Compiling replay mode for region {0} invocation {1} without instrumentation".format(args.region, args.invocation))
    else:
        instru_cmd = "--instrument"
        logger.info("Compiling replay mode for region {0} invocation {1} with instrumentation".format(args.region, args.invocation))
    try:
        logger.debug(subprocess.check_output("{0} INVITRO_CALL_COUNT={5} MODE=\"replay --region={1} --invocation={2} {3} --wrapper={4}\" -B".format(cere_configure.cere_config["build_cmd"], args.region, args.invocation, instru_cmd, args.wrapper, args.invitro_callcount), stderr=subprocess.STDOUT, shell=True))
    except subprocess.CalledProcessError as err:
        logger.error(str(err))
        logger.error(err.output)
        logger.error("Compiling replay mode for region {0} invocation {1} Failed".format(args.region, args.invocation))
        utils.mark_invalid(args.region, cere_error.EREPLAY)
        return False
    if not args.norun:
        logger.info("Replaying invocation {1} for region {0}".format(args.region, args.invocation))
        try:
            logger.debug(subprocess.check_output(cere_configure.cere_config["run_cmd"], stderr=subprocess.STDOUT, shell=True))
        except subprocess.CalledProcessError as err:
            logger.error(str(err))
            logger.error(err.output)
            logger.error("Replay failed for {0} invocation {1}".format(args.region, args.invocation))
            utils.mark_invalid(args.region, cere_error.EREPLAY)
            return False
    return True
