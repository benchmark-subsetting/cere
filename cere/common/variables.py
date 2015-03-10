#!/usr/bin/env python

import os

PROJECT_ROOT = os.path.dirname(os.path.realpath(__file__+"/../.."))
RDTSC_LIB = PROJECT_ROOT + "/src/rdtsc/librdtsc.a"
RDTSC_WRAPPER = PROJECT_ROOT + "/src/ccc/lel/librdtsc_wrapper.a"
