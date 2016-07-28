# This file is part of CERE.
#
# Copyright (c) 2013-2016, Universite de Versailles St-Quentin-en-Yvelines
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

import vars as var
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

def dump_exist(region, invocation):
  if os.path.isdir(os.path.join(var.CERE_DUMPS_PATH, region, str(invocation))):
    return True

class Error_table:
    def __init__(self):
        self.table = []
        self.complete_error_table(0, 0)

    def complete_error_table(self, error, coverage):
        self.table = self.table + [[error,coverage]]

    def write_table(self, error_file):
        output = open(error_file,'w')
        output.write("Error,Exec Time\n")
        for c in self.table:
            output.write(str(c[0]) + "," + str(c[1]) + "\n")
