#!/usr/bin/env python

import variables as var
import os

def is_invalid(r):
    if not os.path.isfile("{0}/{1}".format(var.CERE_REPLAY_PATH, var.INVALID_REGION_FILE)): return False
    with open("{0}/{1}".format(var.CERE_REPLAY_PATH, var.INVALID_REGION_FILE), 'r') as invalid_file:
        for line in invalid_file:
            region, message = line.split(" ", 1)
            if r == region.strip(): return True
    return False

def mark_invalid(r, err_message = ""):
    with open("{0}/{1}".format(var.CERE_REPLAY_PATH, var.INVALID_REGION_FILE), 'a') as invalid_file:
        if not is_invalid(r):
            invalid_file.write(r+" "+err_message+"\n")

def get_error_message(r):
    if not os.path.isfile("{0}/{1}".format(var.CERE_REPLAY_PATH, var.INVALID_REGION_FILE)): return "Unknown Error"
    with open("{0}/{1}".format(var.CERE_REPLAY_PATH, var.INVALID_REGION_FILE), 'r') as invalid_file:
        for line in invalid_file:
            region, message = line.split(" ", 1)
            if r == region.strip(): return message
    return "Unknown Error"

def trace_exists(region):
    return (os.path.isfile("{0}/{1}.csv".format(var.CERE_TRACES_PATH, region))
            and os.path.isfile("{0}/{1}.bin".format(var.CERE_TRACES_PATH, region)))

def io_trace_exists(region, invocation):
    trace_dir = os.path.join(var.CERE_IO_TRACES_PATH, region, str(invocation))
    return os.path.isfile("{0}/{1}".format(trace_dir, region))

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
