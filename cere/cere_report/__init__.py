#!/usr/bin/env python

import os
import sys
import cPickle as pickle
import networkx as nx
import cere_configure
import logging
import argparse
import jinja2
import csv
import base64
from contextlib import contextmanager

CSV_DELIMITER = ','
LIST_PREFIX = ["__invivo__","__extracted__"]
ROOT = os.path.dirname(os.path.realpath(__file__))
NAME_FILE = "regions.csv"
REGIONS_FIELDNAMES = ["Exec Time (%)", "Codelet Name", "Error (%)"]
INVOCATION_FIELDNAMES = ["Invocation", "Cluster", "Part", "Invitro (cycles)",
                         "Invivo (cycles)", "Error (%)"]

Mode_dict = {".c":["clike/clike.js","text/x-csrc"], ".C":["clike/clike.js",
             "text/x-csrc"], ".f":["fortran/fortran.js", "text/x-Fortran"],
             ".F":["fortran/fortran.js", "text/x-fortran"],
             ".f90":["fortran/fortran.js", "text/x-fortran"],
             ".F90":["fortran/fortran.js", "text/x-fortran"],
             ".html":["htmlmixed/htmlmixed.js", "text/htmlmixed"],
             ".cc":["clike/clike.js", "text/x-c++src"],
             ".cpp":["clike/clike.js", "text/x-c++src"]}

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
    def __init__(self, region):
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
        self._table = {"Exec Time (%)":float(region['_self_coverage']), "Error (%)":float(region["_error"]),
                       "Codelet Name":suppr_prefix(region["_name"])}
        self._inv_table = []
        self._code = Code(".html", "CODE NOT FOUND -> THIS CODELET NOT IN regions.csv?", 1)
        self._callcount = 0
        self._selected = "false"
        self._tooSmall = "false"
        self._execError = "false"
        self.init_graph()
    
    def init_graph(self):
        self._graph_clustering = encode_graph("/{region}_byPhase.png".format(region=self._name.replace("extracted", "invivo")))
        
    def set_callcount(self,callcount):
        self._callcount = callcount
        
    def append_invocation_table(self,inv):
        self._inv_table = inv
        #~ self._inv_table +[ {"Cluster":inv["Cluster"], "Invocation":inv["Invocation"],
                          #~ "Part":inv["Part"], "Invivo (cycles)":"{:e}".format(float(inv["Invivo"])),
                          #~ "Invitro (cycles)":"{:e}".format(float(inv["Invitro"])), "Error (%)":float(inv["Error"])}]
        
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
        self.init_template()
        self.init_nb_cycles()
        self.init_regions(graph, mincycles)
        self.init_liste_script()
        self.init_tree()
        self.init_part()
        self.init_graph_error()
        self.init_javascript()
        
    def init_template(self):
        '''
        Read the template in Report/template.html
        '''
        try:
            TEMPLATE=open(ROOT + "/template.html", 'r')
        except (IOError) as err:
            logging.critical("Can't open template.html: {0}",err)
            sys.exit(1)
        self._template = jinja2.Template(TEMPLATE.read())
        TEMPLATE.close()
        
    def init_nb_cycles(self):
        '''
        Read the nb_cycles value of application in app_cycles.csv
        '''
        Dict = read_csv(cere_configure.cere_config["cere_measures_path"] + '/app_cycles.csv')
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
            self._regions[suppr_prefix(d['_name'])] = Region(d)
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
                infos = csv.reader(open(cere_configure.cere_config["cere_measures_path"]+"/__invivo__{0}.csv".format(k)))
                line = infos.next()
                line = infos.next()
            except (IOError):
                pass
            else:
                try:
                    self._regions[k].set_callcount(line[1])
                except(KeyError):
                    logging.info("CALL_COUNT: " + suppr_prefix(loop["Codelet Name"]) + " not in matching error")
        
    def init_invocation_table(self, graph):
        '''
        Initialize the information about the different invocation of regions
        We read invocations_error and append invocation information for the region in column "Codelet Name"
        '''
        for n, d in graph.nodes(data=True):
            try:
                self._regions[suppr_prefix(d['_name'])].append_invocation_table(d['_invocations'])
            except(KeyError):
                logging.info("INVOCATIONS: " + suppr_prefix(suppr_prefix(d['_name'])) + " not in graph")
        
    def init_codes(self):
        '''
        Initialize the list of Code.
        For each region we extract code in file to print them in Report
        '''
        try:
            FILE = open(NAME_FILE, 'rb')
        except (IOError):
            logging.info("Can't read " + NAME_FILE + "-> Verify coverage and matching")
        table = csv.reader(FILE, delimiter=CSV_DELIMITER)
        for code_place in table:
            try:
                if (suppr_prefix(code_place[0]) in self._regions):
                    self._regions[suppr_prefix(code_place[0])].init_code(code_place)
            except(KeyError):
                logging.info("CODE_PLACE: " + suppr_prefix(code_place[0]) + " not in matching error")
        
    def init_liste_script(self):
        '''
        Initialize the list of script include in template for codemirror.
        There is one script for each code language 
        '''
        self._liste_script = []
        for region in self._regions:
            self._liste_script = self._liste_script + [self._regions[region]._code._script]
        self._liste_script = set(self._liste_script)
        
    def init_tree(self):
        '''
        Initialize the tree given by selected_codelets
        For each line of selected_codelets create a Node object with the region and node information
        '''
        self._tree = []
        tree = read_csv(cere_configure.cere_config["cere_measures_path"] + "/selected_codelets")
        if not tree: return False
        for node in tree:
            try:
                if (suppr_prefix(node["Codelet Name"]) in self._regions):
                    region = self._regions[suppr_prefix(node["Codelet Name"])]
                    region.init_selected(node["Selected"])
                    self._tree = self._tree + [Node(node,region)]
            except(KeyError):
                logging.info("SELECTED_CODELETS: " + suppr_prefix(node["Codelet Name"]) + " not in selected codelets")
        self.test_parent_tree()
        self.remove_loops()
        
    def test_parent_tree(self):
        '''
        Verify for each child in tree that his parent is in the tree
        If not the child._parent is changed to "none"
        '''
        if (len(self._tree) == 0):
            raise MyError("/selected_codelets empty")
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
            if node._region._tooSmall == "false" and node._region._execError == "false" and node not in self.tmp_tree:
                self.tmp_tree = self.tmp_tree + [node]
                while node._parent != "none":
                    parent = self.get_node_by_id(node._parent, self._tree)
                    if parent._region._execError == "true":
                        node = self.tmp_tree[node]
                        granpa = self.get_node_by_id(parent._parent)
                        node._parent = granpa._id if granpa != "none" else "none"
                    else:
                        node = parent
                        if node not in self.tmp_tree: self.tmp_tree = self.tmp_tree + [node]
                        else: break
        self._tree = self.tmp_tree

    def get_node_by_id(self, _id, tree):
        for node in tree:
            if node._id == _id:
                return node
        return "none"

    def init_part(self):
        '''
        Compute coverage of selected regions 
        See all region and add region's coverage if the region is selected
        '''
        self._part = 0
        for region in self._regions:
            if(self._regions[region]._selected == "true"):
                self._part = self._part + self._regions[region]._table["Exec Time (%)"]

    def init_graph_error(self):
        '''
        Read bench_error.png
        '''
        self._graph_error = encode_graph("/bench_error.png")

    def init_javascript(self):
        '''
        Read javascript file : Report/Report.js
        '''
        try:
            with file(ROOT + '/Report.js') as jsf:
                self.javascript=jsf.read()
        except (IOError):
            raise MyError("Cannot find Report.js")

    def write_report(self):
        '''
        Write report by passing information to the template
        '''
        try:
            REPORT=open(self._bench+'.html','w')
        except (IOError):
            raise MyError("Cannot open "+ self._bench +".html")
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
    os.system(ROOT + "/../../src/granularity/graph_error.R")
    #try:
    yield
    #except MyError as err:
    #    exit("Error Report -> " + err.value)
    #else:
    print ("Report created")
    os.chdir(TEMP_DIR)

def read_csv(File):
    try:
        FILE = open(File, 'rb')
    except (IOError):
        logging.critical("Can't read " + File + "-> Verify coverage and matching")
        return False
    Dict = csv.DictReader(FILE, delimiter=CSV_DELIMITER)
    return Dict

def encode_graph(graph_name):
    '''
    Read graph and encode this graph in base 64.
    '''
    try:
        with open(cere_configure.cere_config["cere_measures_path"] + "/plots/" + graph_name, "rb") as f:
            graph = f.read()
    except (IOError):
        logging.info("Cannot find " + graph_name)
    else: return base64.standard_b64encode(graph)

def percent(x):
    return (100 * float(x))

def suppr_prefix(name):
    '''
    Remove prefix of a region name
    '''
    for pre in LIST_PREFIX:
        name = name.replace(pre,"")
    return name

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

def load_graph():
    logging.info('Loading graph...')
    graph = None
    with open("{0}/graph.pkl".format(cere_configure.cere_config["cere_measures_path"]), 'rb') as input:
        graph = pickle.load(input)
    return graph

def run(args):
    logging.info('CERE Report')
    cere_configure.init()
    graph = load_graph()
    if graph == None:
        logging.critical("Can't load graph. Did you run cere filter?")
        return False

    with context(args.path):
        bench = os.getcwd().split("/") [-1]
        REPORT = Report(bench, graph, args.mincycles)
        REPORT.write_report()
    return True
