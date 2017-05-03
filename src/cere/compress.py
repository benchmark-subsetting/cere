#!/usr/bin/env python
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

import os
import sys

PAGESIZE = os.sysconf("SC_PAGE_SIZE")

def compress(dir):
    owd = os.getcwd()
    os.chdir(dir)

    dumps = []

    for f in os.listdir("."):
        if f.endswith(".memdump"):
            dumps.append(int(f.split(".")[0], 16))

    dumps.sort()

    def consecutive(n,m):
        return abs(dumps[n]-dumps[m]) == PAGESIZE

    ranges = []

    start = 0
    last = 0
    for i in range(1, len(dumps)):
        if consecutive(last, i):
            last = i
        else:
            ranges.append((start, last))
            start = i
            last = i

    ranges.append((start, last))

    for ran in ranges:
        start, stop = ran
        with open("{0:012x}.memdump".format(dumps[start]), "ab") as first:
            for i in range(start+1, stop+1):
                other_name = "{0:012x}.memdump".format(dumps[i])
                with open(other_name, "rb") as other:
                    first.write(other.read(PAGESIZE))
                os.remove(other_name)

    print(ranges)

    os.chdir(owd)
