#!/usr/bin/env python

import cere_configure
import os
PREFIX_LIST = ["__invivo__","__extracted__"]

def suppr_prefix(region):
    for pre in PREFIX_LIST:
        region = region.replace(pre, "")
    return region

def is_invalid(r):
    r = suppr_prefix(r)
    if not os.path.isfile("{0}/invalid_loops".format(cere_configure.cere_config["cere_measures_path"])): return False
    with open("{0}/invalid_loops".format(cere_configure.cere_config["cere_measures_path"]), 'r') as invalid_file:
        for region in invalid_file:
            if r == region: return True
    return False

def mark_invalid(r):
    r = suppr_prefix(r)
    with open("{0}/invalid_loops".format(cere_configure.cere_config["cere_measures_path"]), 'a') as invalid_file:
        if not is_invalid(r):
            invalid_file.write(r)
