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
# CERE is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with CERE.  If not, see <http://www.gnu.org/licenses/>.

import os
import sys
import argparse
import logging
import shutil
import subprocess
import cere_configure
import cere_replay
import common.variables as var
import common.utils as utils
import common.errors as cere_error

logger = logging.getLogger('Flag')

def init_module(subparsers, cere_plugins):
  cere_plugins["flag"] = run
  flag_parser = subparsers.add_parser("flag", help="test flags performance")
  flag_parser.add_argument('--region', required=True, help="region to replay")
  flag_parser.add_argument('--flags', required=True, help="flags to test, comma separated")
  flag_parser.add_argument('--invocation', type=int, help="invocation to replay")
  flag_parser.add_argument('--invitro-callcount', type=int, default=10, help="Meta-repetition for the replay (Default 10)")
  flag_parser.add_argument('--plugin-instr', default=var.RDTSC_WRAPPER, help="Plugin to instrument the replay")
  flag_parser.add_argument('--noinstrumentation', action='store_true', help="Replay without instrumentation")
  flag_parser.add_argument('--norun', action='store_true', help="If you don't want to automatically run the replay")
  flag_parser.add_argument('--force', '-f', action='store_true', help="force to replay (Delete previous measure)")

def run(args):
  flags = args.flags.split(',')
  for f in flags:
    logger.info("Measuring with {0} flag".format(f))
    os.environ["CERE_BACKEND_FLAGS"] = f
    cere_replay.run(args)

  if "CERE_BACKEND_FLAGS" in os.environ:
    del os.environ["CERE_BACKEND_FLAGS"]
  return True
