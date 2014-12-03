#!/usr/bin/env python

import os
import shutil
import sys
import json
import argparse
import logging

cere_config={}

def init_module(subparsers, cere_plugins):
    cere_plugins["configure"] = run
    configure_parser = subparsers.add_parser("configure", help="Configure CERE to works with your application")
    configure_parser.add_argument('--run_cmd', required=True, help="Set the command to run your application")
    configure_parser.add_argument('--build_cmd', required=True, help="Set the command to build the application")
    configure_parser.add_argument('--measures_path', default="cere_measures", help="Directory where to save CERE measures")
    configure_parser.add_argument('--dumps_path', default="cere_dumps", help="Directory where to save CERE regions dumps")

def run(args):
    global cere_config
    cere_config["build_cmd"] = args.build_cmd
    cere_config["run_cmd"] = args.run_cmd
    cere_config["cere_measures_path"] = args.measures_path
    cere_config["cere_dumps_path"] = args.dumps_path

    with open("cere.json", 'w') as config_file:
        json.dump(cere_config, config_file)
    logging.info("Configuration done")
    return True

def init():
    global cere_config
    if not os.path.isfile("cere.json"):
        logging.critical("No configure file found, please first run cere configure")
        sys.exit(1)
    with open("cere.json", 'r') as config_file:
        cere_config = json.load(config_file)
    if not setup_dir(cere_config["cere_measures_path"]): sys.exit(1)

def query_yes_no(question, default="yes"):
    """Ask a yes/no question via raw_input() and return their answer.

    "question" is a string that is presented to the user.
    "default" is the presumed answer if the user just hits <Enter>.
        It must be "yes" (the default), "no" or None (meaning
        an answer is required of the user).

    The "answer" return value is one of "yes" or "no".
    """
    valid = {"yes": True, "y": True, "ye": True,
             "no": False, "n": False}
    if default is None:
        prompt = " [y/n] "
    elif default == "yes":
        prompt = " [Y/n] "
    elif default == "no":
        prompt = " [y/N] "
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while True:
        sys.stdout.write(question + prompt)
        choice = raw_input().lower()
        if default is not None and choice == '':
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            sys.stdout.write("Please respond with 'yes' or 'no' "
                             "(or 'y' or 'n').\n")

def setup_dir(measures_path):
    if not os.path.isdir(measures_path):
        try:
            os.mkdir(measures_path)
        except OSError as err:
            logging.critical(str(err))
            return False
    if not os.path.isdir("{0}/plots".format(measures_path)):
        try:
            os.mkdir("{0}/plots".format(measures_path))
        except OSError as err:
            logging.critical(str(err))
            return False
    return True

#~ def clean_dir(clean_measures, clean_dumps):
    #~ if clean_measures:
        #~ if os.path.isdir(default_measures_path):
            #~ if query_yes_no("WARNING: Are you sure you want to remove CERE results?"):
                #~ shutil.rmtree(default_measures_path)
        #~ else:
            #~ print("No previous CERE measures")
    #~ if clean_dumps:
        #~ if os.path.isdir(default_dumps_path):
            #~ if query_yes_no("WARNING: Are you sure you want to remove CERE dumps?"):
                #~ shutil.rmtree(default_dumps_path)
        #~ else:
            #~ print("No previous CERE dumps")
