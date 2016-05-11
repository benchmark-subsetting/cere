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
#!/usr/bin/env python

import os

def create_path(top_dir, sub_dir):
  new_path = os.path.join(top_dir, sub_dir)
  CERE_DIRECTORIES.append(new_path)
  return new_path

PROJECT_ROOT = os.path.dirname(os.path.realpath(__file__+"/../../../"))
RDTSC_WRAPPER = PROJECT_ROOT + "/src/rdtsc/librdtsc.a"
FLAGS_FILE = PROJECT_ROOT + "/src/cere/common/flags.csv"
IO_CHECKER_LIB = PROJECT_ROOT + "/src/io_detector/libio_detector.a"
INVALID_REGION_FILE = "invalid_regions"

CERE_DIRECTORIES = []

CERE_MAIN_DIR = ".cere"
CERE_REPLAYS = "replays"
CERE_PROFILE = "profile"
CERE_TRACES = "traces"
CERE_DUMPS = "dumps"
CERE_PLOTS = "plots"
CERE_REPORT = "report"
CERE_IO_TRACES = "io_traces"

CERE_REPLAY_PATH = create_path(CERE_MAIN_DIR, CERE_REPLAYS)
CERE_PROFILE_PATH = create_path(CERE_MAIN_DIR, CERE_PROFILE)
CERE_TRACES_PATH = create_path(CERE_MAIN_DIR, CERE_TRACES)
CERE_DUMPS_PATH = create_path(CERE_MAIN_DIR, CERE_DUMPS)
CERE_PLOTS_PATH = create_path(CERE_MAIN_DIR, CERE_PLOTS)
CERE_REPORT_PATH = create_path(CERE_MAIN_DIR, CERE_REPORT)
CERE_IO_TRACES_PATH = create_path(CERE_MAIN_DIR, CERE_IO_TRACES)
