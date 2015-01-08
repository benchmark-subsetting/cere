#!/usr/bin/env python

import os
#~ import subprocess
#~ import sys
import cere_configure
import cere_dump
import logging
#~ import shutil
import argparse

def init_module(subparsers, cere_plugins):
    cere_plugins["regions"] = run
    profile_parser = subparsers.add_parser("regions", help="List extractible loops")
    profile_parser.add_argument("--static", nargs='?', const=True, default=True, help="List loops")
    profile_parser.add_argument("--dynamic", nargs='?', const=True, default=False, help="List loops with coverage")
    profile_parser.add_argument('--force', '-f', const=True, default=False, nargs='?', help="Will overwrite previous loop list file")

class Dump():
    def __init__(self, f):
        self.force = f
        self.norun = True
        self.region = False

def run(args):
    cere_configure.init()
    if(args.force):
        logging.info("Removing previous regions list")
        if os.path.isfile("regions.csv"):
            os.remove("regions.csv")
    if(args.dynamic):
        logging.info("Listing loops with coverage")
    elif(args.static):
        mydump = Dump(args.force)
        res = cere_dump.run(mydump)
        logging.info("Regions list dumped in regions.csv file")
        if not res:
            logging.critical("Regions listing encountered and error: list may be incomplete or empty")
            return False
    return True

