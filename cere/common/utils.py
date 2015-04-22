#!/usr/bin/env python

import cere_configure
import variables as var
import os

def is_invalid(r):
    if not os.path.isfile("{0}/{1}".format(cere_configure.cere_config["cere_measures_path"], var.INVALID_REGION_FILE)): return False
    with open("{0}/{1}".format(cere_configure.cere_config["cere_measures_path"], var.INVALID_REGION_FILE), 'r') as invalid_file:
        for region in invalid_file:
            if r == region.rstrip(): return True
    return False

def mark_invalid(r):
    with open("{0}/{1}".format(cere_configure.cere_config["cere_measures_path"], var.INVALID_REGION_FILE), 'a') as invalid_file:
        if not is_invalid(r):
            invalid_file.write(r+"\n")

def trace_exists(region):
    return (os.path.isfile("{0}/{1}.csv".format(cere_configure.cere_config["cere_measures_path"], region))
            and os.path.isfile("{0}/{1}.bin".format(cere_configure.cere_config["cere_measures_path"], region)))

class Error_table:
    def __init__(self):
        self.table = []

    def complete_error_table(self, error, coverage):
        self.table = self.table + [[error,coverage]]

    def write_table(self, error_file):
        output = open(error_file,'w')
        output.write("Error,Exec Time\n")
        for c in self.table:
            output.write(str(c[0]) + "," + str(c[1]) + "\n")
