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
import cPickle as pickle
import networkx as nx
import cere_configure
from graph_utils import *
import logging
import argparse
import jinja2
import csv
import base64
import xmlrpclib
import vars as var
from contextlib import contextmanager
import graph_error

logger = logging.getLogger('Report')

CSV_DELIMITER = ','
ROOT = os.path.dirname(os.path.realpath(__file__))
NAME_FILE = "regions.csv"
REGIONS_FIELDNAMES = ["Self (%)", "Cumulative (%)", "Codelet Name", "Error (%)"]
INVOCATION_FIELDNAMES = ["Invocation", "Cluster", "Part", "Invitro (cycles)",
                         "Invivo (cycles)", "Error (%)"]

Mode_dict = {".c":["clike/clike.js","text/x-csrc"], ".C":["clike/clike.js",
             "text/x-csrc"], ".f":["fortran/fortran.js", "text/x-Fortran"],
             ".F":["fortran/fortran.js", "text/x-fortran"],
             ".f90":["fortran/fortran.js", "text/x-fortran"],
             ".F90":["fortran/fortran.js", "text/x-fortran"],
             ".html":["htmlmixed/htmlmixed.js", "text/htmlmixed"],
             ".cc":["clike/clike.js", "text/x-c++src"],
             ".cpp":["clike/clike.js", "text/x-c++src"],
             ".h":["clike/clike.js","text/x-csrc"]}

def init_module(subparsers, cere_plugins):
    cere_plugins["report"] = run
    profile_report = subparsers.add_parser("report", help="Generates the html report for an application")
    profile_report.add_argument("--path", '-p', default=".", help="Path to the application to report. (Default is current folder)")
    profile_report.add_argument("--mincycles", type=int, default=0, help="Minimum cycles per invocation for a region. (Default is 0)")

class Code:
    def __init__(self, ext, value, line):
        '''
        Initialize the information of a Code
        value: text of code
        line: line of function in text code
        mode and script: information for codemirror depends on language of text code
        '''
        self._value = value
        self._line = line
        self._mode = Mode_dict[ext][1]
        self._script = Mode_dict[ext][0]
    def getScript(self):
        return self._script

class Region:
    def __init__(self, region, graph):
        '''
        Initialize the Region
        name: Region name
        invivo: Exec time in invivo mode
        invitro: Exec time in invitro mode
        table: row for the main table in report
        inv_table: table with invocation information
        code: Objet code with code information
        callcount: region callcount
        selected: if region is selected with matching script
        tooSmall: if nb cycles per invocation < mincycles we won't print this region
        execError: if the codelet failed to replay
        graph_clustering: graph with the clustering of invocation
        graph_invoc: graph with the different invocation
        '''
        self._name = region["_name"]
        self._invivo = "{:e}".format(float(region['_invivo']))
        self._invitro = "{:e}".format(float(region['_invitro']))
        self._table = {"Self (%)":round(float(region['_self_coverage']), 2), "Error (%)":round(float(region["_error"]), 2),
                       "Codelet Name":region["_name"], "Cumulative (%)":round(float(region['_coverage']), 2), "Error message":region['_error_message']}
        self._inv_table = []
        self._code = Code(".html", "CODE NOT FOUND -> THIS CODELET NOT IN regions.csv?", 1)
        self._callcount = 0
        self._selected = "false"
        if region['_small']:
            self._tooSmall = "true"
        else: self._tooSmall = "false"
        self._execError = "false"
        self.init_graph()
        self.init_call_graph(graph)

    def plot_call_graph(self, g):
        for n,d in g.nodes(data=True):
            d["label"]="{} {} {} ({})".format(n, d['_name'], d['_self_coverage'], d['_coverage'])
            if not d['_valid'] or d['_small']: d["style"]="dotted"
            else: d["style"]="solid"
            if d['_tested']:
                d["style"]="solid"
                if not d['_matching']: d['color']="red"
                else: d['color']="green"
            if d['_to_test']: d['color']="orange"
            if d['_name'] == self._name: d['style']="filled"
        nx.write_dot(g,"{0}/graph_{1}.dot".format(var.CERE_PLOTS_PATH, self._name))
        try:
            subprocess.check_output("dot -Tpng {0}/graph_{1}.dot -o {0}/graph_{1}.png".format(var.CERE_PLOTS_PATH, self._name), stderr=subprocess.STDOUT, shell=True)
        except subprocess.CalledProcessError as err:
            logger.error(str(err))
            logger.error(err.output)
            logger.warning("Can't create call graph fo region {0}".format(self._name))

    def init_graph(self):
        if self._tooSmall == "false":
            self._graph_clustering = encode_graph("{0}/{1}_byPhase.png".format(var.CERE_PLOTS_PATH, self._name))

    def init_call_graph(self, graph):
        if self._tooSmall == "false":
            self.plot_call_graph(graph)
            self._call_graph = encode_graph("{0}/graph_{1}.png".format(var.CERE_PLOTS_PATH, self._name))

    def set_callcount(self,callcount):
        self._callcount = callcount

    def append_invocation_table(self,inv):
        self._inv_table = inv

    def init_code(self, code_place):
        self._code = read_code(code_place)

    def init_selected(self, selected):
        self._selected = selected

class Node:
    def __init__(self,node,region):
        '''
        Initialize the information of a node
        region: object Region contained by the Node
        id: id of the node
        parent: id of the Node's Parent
        selected: True if the Node is selected by matching algorithm
        '''
        self._region = region
        self._parent = node["ParentId"]
        self._selected = node["Selected"]
        self._id = node["Id"]

unique_id = 0

def get_uid():
    global unique_id
    unique_id += 1
    return str(unique_id)

class Report:
    def __init__(self, bench, graph, mincycles):
        '''
        Initialize the Report
        bench: bench Name
        template: Report template
        nb_cycles: nb_cycles of bench
        regions: list of analyzed regions.
                 Analyzed regions are the regions with all results
        liste_script: list of script javascript for codemirror to print text code
        tree: tree of regions to make the main table in report
        part: coverage of application with selected region
        javascript: main script javascript for report
        '''
        self._bench = bench
        self._part = graph.graph['coverage']
        self.init_template()
        self.init_nb_cycles()
        self.init_regions(graph, mincycles)
        self.init_liste_script()
        self.output_tree(graph)
        self.init_tree()
        self.init_graph_error()
        self.init_javascript()

    def init_template(self):
        '''
        Read the template in Report/template.html
        '''
        try:
            TEMPLATE=open(ROOT + "/template.html", 'r')
        except (IOError) as err:
            logger.critical("Cannot open template.html: {0}", str(err))
            sys.exit(1)
        self._template = jinja2.Template(TEMPLATE.read())
        TEMPLATE.close()

    def init_nb_cycles(self):
        '''
        Read the nb_cycles value of application in app_cycles.csv
        '''
        if not os.path.isfile("{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH)):
            logger.error("Profile file missing. Please run cere profile")
            self._nb_cycles = 0
        else:
            Dict = read_csv("{0}/app_cycles.csv".format(var.CERE_PROFILE_PATH))
            try:
                row = Dict.next()
                self._nb_cycles = row["CPU_CLK_UNHALTED_CORE"]
                self._nb_cycles = "{:e}".format(int(self._nb_cycles))
            except (StopIteration):
                raise MyError("/app_cycles.csv empty")
            except (KeyError):
                raise MyError("error key: not CPU_CLK_UNHALTED_CORE in /app_cycles.csv ")

    def init_regions(self, graph, mincycles):
        '''
        Initialize the region list
        For each region in matching_error we create a new objet Region, which contains all information neccessary
        for the report about this region
        Then we initialize the value not in matching_error.csv like callcount
        '''
        self._regions = {}
        for n,d in graph.nodes(data=True):
            self._regions[d['_name']] = Region(d, graph)
        self.init_callcount()
        for k,r in self._regions.iteritems():
            if r._table["Error (%)"] == 100:
                self._regions[k]._execError = "true"
        self.init_invocation_table(graph)
        self.init_codes()

    def init_callcount(self):
        '''
        Initialize the callcount
        We read all files level_*.csv and for each region in each file we initialize his callcount with the value in the file
        '''
        for k,r in self._regions.iteritems():
            try:
                infos = csv.reader(open("{0}/{1}.csv".format(var.CERE_TRACES_PATH, k)))
                line = infos.next()
                line = infos.next()
            except (IOError):
                pass
            else:
                try:
                    self._regions[k].set_callcount(line[1])
                except(KeyError):
                    logger.warning("CALL_COUNT: " + loop["Codelet Name"] + " not in matching error")

    def init_invocation_table(self, graph):
        '''
        Initialize the information about the different invocation of regions
        We read invocations_error and append invocation information for the region in column "Codelet Name"
        '''
        for n, d in graph.nodes(data=True):
            try:
                self._regions[d['_name']].append_invocation_table(d['_invocations'])
            except(KeyError):
                logger.warning("INVOCATIONS: " + d['_name'] + " not in graph")

    def init_codes(self):
        '''
        Initialize the list of Code.
        For each region we extract code in file to print them in Report
        '''
        try:
            FILE = open(NAME_FILE, 'rb')
        except (IOError):
            logger.error("Can't read " + NAME_FILE + "-> Verify coverage and matching")
        table = csv.reader(FILE, delimiter=CSV_DELIMITER)
        for code_place in table:
            try:
                if (code_place[0] in self._regions):
                    self._regions[code_place[0]].init_code(code_place)
            except(KeyError):
                logger.warning("CODE_PLACE: " + code_place[0] + " not regions")

    def init_liste_script(self):
        '''
        Initialize the list of script include in template for codemirror.
        There is one script for each code language
        '''
        self._liste_script = []
        for region in self._regions:
            self._liste_script = self._liste_script + [self._regions[region]._code._script]
        self._liste_script = set(self._liste_script)

    def output_codelet(self, output, graph, chosen, node, direct_parent_id, parents):
        selected = "true" if node in chosen else "false"
        codelet_id =  get_uid()
        output.write(",".join([codelet_id, graph.node[node]['_name'], selected, direct_parent_id]) + "\n")
        for child_name in graph.successors(node):
            if child_name not in parents:
                self.output_codelet(output, graph, chosen, child_name, codelet_id, parents | set([node]))

    def output_tree(self, graph):
        chosen=[]
        for n, d in graph.nodes(data=True):
            if d["_selected"]: chosen.append(n)
        with open("{0}/selected_codelets".format(var.CERE_REPORT_PATH), 'w') as output:
            # print header
            output.write("Id,Codelet Name,Selected,ParentId\n")
            # find roots
            for n, d in graph.nodes(data=True):
                if not graph.predecessors(n): #root
                    self.output_codelet(output, graph, chosen, n, "None", set())

    def init_tree(self):
        '''
        Initialize the tree given by selected_codelets
        For each line of selected_codelets create a Node object with the region and node information
        '''
        self._tree = []
        tree = read_csv("{0}/selected_codelets".format(var.CERE_REPORT_PATH))
        if not tree: return False
        for node in tree:
            try:
                if (node["Codelet Name"] in self._regions):
                    region = self._regions[node["Codelet Name"]]
                    region.init_selected(node["Selected"])
                    self._tree = self._tree + [Node(node,region)]
            except(KeyError):
                logger.warning("SELECTED_CODELETS: " + node["Codelet Name"] + " not in selected codelets")
        #~self.remove_loops()
        self.test_parent_tree()

    def test_parent_tree(self):
        '''
        Verify for each child in tree that his parent is in the tree
        If not the child._parent is changed to "none"
        '''
        if (len(self._tree) == 0):
            logger.error("No codelets selected")
        for node in self._tree:
            if (node._parent is not "none"):
                orphan = True
                for parent in self._tree:
                    if(parent._id == node._parent):
                        orphan = False
                        break
                if (orphan):
                    node._parent = "none"

    def remove_loops(self):
        '''
        Remove too small loops from the tree.
        '''
        if (len(self._tree) == 0):
            raise MyError("/selected_codelets empty")
        self.tmp_tree = []
        for node in self._tree:
            if node._region._tooSmall == "false" and node not in self.tmp_tree:
                self.tmp_tree = self.tmp_tree + [node]
        self._tree = self.tmp_tree

    def get_node_by_id(self, _id, tree):
        for node in tree:
            if node._id == _id:
                return node
        return "none"

    def init_graph_error(self):
        '''
        Read bench_error.png
        '''
        self._graph_error = encode_graph("{0}/bench_error.png".format(var.CERE_PLOTS_PATH))

    def init_javascript(self):
        '''
        Read javascript file : Report/Report.js
        '''
        try:
            with file(ROOT + '/Report.js') as jsf:
                self.javascript=jsf.read()
        except (IOError):
            raise MyError("Can't find Report.js")

    def write_report(self):
        '''
        Write report by passing information to the template
        '''
        try:
            REPORT=open(self._bench+'.html','w')
        except (IOError):
            raise MyError("Can't open "+ self._bench +".html")
        REPORT.write(self._template.render(bench=self._bench, root=ROOT, nb_cycles=self._nb_cycles,
                     regionlist=self._regions, regionfields=REGIONS_FIELDNAMES, tree = self._tree,
                     invocationfields=INVOCATION_FIELDNAMES, l_modes=self._liste_script,
                     report_js=self.javascript, part=self._part, graph_error=self._graph_error))
        REPORT.close()

@contextmanager
def context(DIR):
    '''
    Change directory to wanted directory
    Rechange to original directory at the end
    '''
    TEMP_DIR = os.getcwd()
    if(os.chdir(DIR)):
        exit("Error Report -> Can't find " + DIR)
    if not os.path.isfile("{0}/table_error.csv".format(var.CERE_REPORT_PATH)):
        logger.warning("Can't find {0}/table_error.csv".format(var.CERE_REPORT_PATH))
    else:
        graph_error.plot_graph("{0}/table_error.csv".format(var.CERE_REPORT_PATH))
    #try:
    yield
    #except MyError as err:
    #    exit("Error Report -> " + err.value)
    #else:
    os.chdir(TEMP_DIR)

def read_csv(File):
    try:
        FILE = open(File, 'rb')
    except (IOError):
        logger.error("Can't read " + File + "-> Verify coverage and matching")
        return False
    Dict = csv.DictReader(FILE, delimiter=CSV_DELIMITER)
    return Dict

def encode_graph(graph_name):
    '''
    Read graph and encode this graph in base 64.
    '''
    try:
        with open(graph_name, "rb") as f:
            graph = f.read()
    except (IOError):
        logger.warning("Can't find " + graph_name)
    else: return base64.standard_b64encode(graph)

def percent(x):
    return (100 * float(x))

def read_code(code_place):
    '''
    Create Code Object with information in code place
    code_place[0] contains region name
    code_place[2] contains file name of wanted region
    code_place[4] contains line of wanted region
    code_place[1] and code_place[3] are not used in Report
    '''
    fileName, fileExtension = os.path.splitext(code_place[2])
    try:
        FILE = open(code_place[2], "r")
        code = FILE.readlines()
        code = "".join(code)
        try:
            code.encode('utf-8')
        except (UnicodeDecodeError):
            return Code(".html", "ERROR UNICODE -> FILE:" +code_place[2], 1)
        FILE.close()
    except (IOError):
        raise MyError("Can't open: "+code_place[2])
        pass
    return Code(fileExtension, code, code_place[4])

def run(args):
    logger.info('Start report')
    if not cere_configure.init():
        return False
    graph = load_graph()
    if graph == None:
        logger.critical("Can't load graph. Did you run cere profile?")
        return False

    with context(args.path):
        bench = os.getcwd().split("/") [-1]
        REPORT = Report(bench, graph, args.mincycles)
        REPORT.write_report()
    logger.info("Report created")
    return True
