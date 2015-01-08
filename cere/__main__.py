#!/usr/bin/env python

import sys
import os
import argparse

import cere_configure
import cere_profile
import cere_dump
import cere_replay
import cere_test
import cere_filter
import cere_trace
import cere_regions
import cere_sanity_check

import logging

cere_plugins = {}

def start_log():
    logging.basicConfig(format='%(levelname)s %(asctime)s %(message)s', datefmt='%m/%d/%Y %I:%M:%S', level=logging.DEBUG)
    logging.info('CERE start')

parser = argparse.ArgumentParser(description="CERE command line")
subparsers = parser.add_subparsers(help="Call CERE modules", dest="mode")

start_log()

cere_configure.init_module(subparsers, cere_plugins)
cere_profile.init_module(subparsers, cere_plugins)
cere_dump.init_module(subparsers, cere_plugins)
cere_replay.init_module(subparsers, cere_plugins)
cere_test.init_module(subparsers, cere_plugins)
cere_filter.init_module(subparsers, cere_plugins)
cere_trace.init_module(subparsers, cere_plugins)
cere_sanity_check.init_module(subparsers, cere_plugins)
cere_regions.init_module(subparsers, cere_plugins)

if __name__ == "__main__":
    args = parser.parse_args()

    if not cere_plugins[args.mode](args):
        print("An error occured, see cere.log for more detailed information")

    logging.info('CERE stop')
