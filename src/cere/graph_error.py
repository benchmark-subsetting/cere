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
import vars as var
import numpy as np
import matplotlib.pyplot as plt

def read_datafile(file_name):
  table = np.genfromtxt(file_name, delimiter=',', names=['error', 'coverage'])
  return table

def plot_graph(datafile):
  table = read_datafile(datafile)
  #Sort by error
  table.sort(0)
  f, ax = plt.subplots(1, figsize=(10, 4.16))
  ax.plot(table['error'], table['coverage'], linewidth=0.8, marker="o")
  ax.axvline(15, color="r", linestyle="dotted", linewidth=1.5)
  ax.grid('on')
  ax.set_ylabel("Exec time Cumulated (%)")
  ax.set_xlabel("Error max allowed (%)")
  ax.axis([0, 105, 0, 105])
  f.savefig(os.path.join(var.CERE_PLOTS_PATH, "bench_error.png"),
          bbox_inches='tight', dpi=100)
