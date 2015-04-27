#!/usr/bin/env python

import os

PROJECT_ROOT = os.path.dirname(os.path.realpath(__file__+"/../.."))
RDTSC_WRAPPER = PROJECT_ROOT + "/src/rdtsc/librdtsc.a"
IO_CHECKER_LIB = PROJECT_ROOT + "/src/io_detector/libio_detector.a"
INVALID_REGION_FILE = "invalid_regions"
