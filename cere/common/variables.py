#!/usr/bin/env python

import os

PROJECT_ROOT = os.path.dirname(os.path.realpath(__file__+"/../.."))
RDTSC_WRAPPER = PROJECT_ROOT + "/src/rdtsc/librdtsc.a"
IO_CHECKER_LIB = PROJECT_ROOT + "/src/io_detector/libio_detector.a"
INVALID_REGION_FILE = "invalid_regions"

CERE_DIR = ".cere"
CERE_REPLAYS = "replays"
CERE_PROFILE = "profile"
CERE_TRACES = "traces"
CERE_DUMPS = "dumps"
CERE_PLOTS = "plots"
CERE_REPORT = "report"

CERE_REPLAY_PATH = os.path.join(CERE_DIR, CERE_REPLAYS)
CERE_PROFILE_PATH = os.path.join(CERE_DIR, CERE_PROFILE)
CERE_TRACES_PATH = os.path.join(CERE_DIR, CERE_TRACES)
CERE_DUMPS_PATH = os.path.join(CERE_DIR, CERE_DUMPS)
CERE_PLOTS_PATH = os.path.join(CERE_DIR, CERE_PLOTS)
CERE_REPORT_PATH = os.path.join(CERE_DIR, CERE_REPORT)
