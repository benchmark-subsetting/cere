#!/usr/bin/env python
# Loop Extractor Compiler
# (C) 2013 University of Versailles
import os
import argparse
import jinja2
import csv
import base64
from contextlib import contextmanager
EXTENSIONS = [".c",".f",".f90",".C",".F",".F90",".cc"]
Mode_dict = {".c":["clike/clike.js","text/x-csrc"], ".C":["clike/clike.js",
             "text/x-csrc"], ".f":["fortran/fortran.js", "text/x-Fortran"],
             ".F":["fortran/fortran.js", "text/x-Fortran"],
             ".f90":["fortran/fortran.js", "text/x-Fortran"],
             ".F90":["fortran/fortran.js", "text/x-Fortran"],
             ".html":["htmlmixed/htmlmixed.js", "text/htmlmixed"],
             ".cc":["clike/clike.js", "text/x-c++src"]}
LIST_PREFIX = ["__invivo__","__extracted__"]
ROOT = os.path.dirname(os.path.realpath(__file__))
ROOT_MEASURE = "./measures"
ROOT_GRAPHS = "./measures/plots"
NAME_FILE = "regions.csv"
CSV_DELIMITER = ','
REGIONS_FIELDNAMES = ["Exec Time (%)", "Codelet Name", "Error (%)"]
INVOCATION_FIELDNAMES = ["Invocation", "Cluster", "Part", "Invitro (cycles)",
                         "Invivo (cycles)", "Error (%)"]
DEBUG_MODE = False


class MyError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)


class Code:
    def __init__(self, ext, value, line):
        self._value = value
        self._line = line
        self._mode = Mode_dict[ext][1]
        self._script = Mode_dict[ext][0]
    def getScript(self):
        return self._script


class Region:
    def __init__(self,region):
        self._name = region["Codelet Name"]
        self._invivo = "{:e}".format(float(region["Invivo"]))
        self._invitro = "{:e}".format(float(region["Invitro"]))
        self._table = {"Exec Time (%)":percent(region["Exec Time"]), "Error (%)":percent(region["Error"]),
                       "Codelet Name":suppr_prefix(region["Codelet Name"])}
        self._inv_table = []
        self._code = Code(".html", "CODE NOT FOUND -> THIS CODELET NOT IN regions.csv?", 1)
        self._callcount = 0
        self.init_graph()
    
    def init_graph(self):
        self._graph_invoc = encode_graph("/{region}.png".format(region=self._name))
        self._graph_clustering = encode_graph("/{region}_byPhase.png".format(region=self._name))
        
    def set_callcount(self,callcount):
        self._callcount = callcount
        
    def append_invocation_table(self,inv):
        self._inv_table = self._inv_table +[ {"Cluster":inv["Cluster"], "Invocation":inv["Invocation"],
                          "Part":inv["Part"], "Invivo (cycles)":"{:e}".format(float(inv["Invivo"])),
                          "Invitro (cycles)":"{:e}".format(float(inv["Invitro"])), "Error (%)":percent(inv["Error"])}]
        
    def init_code(self, code_place):
        self._code = read_code(code_place)
        
    def init_selected(self, selected):
        self._selected = selected


class Node:
    def __init__(self,node,region):
        self._region = region
        self._parent = node["ParentId"]
        self._selected = node["Selected"]
        self._id = node["Id"]


class Report:
    def __init__(self,bench):
        self._bench = bench
        self.init_template()
        self.init_nb_cycles()
        self.init_regions()
        self.init_liste_script()
        self.init_tree()
        self.init_part()
        self.init_graph()
        self.init_javascript()
        
    def init_template(self):
        try:
            TEMPLATE=open(ROOT + "/template.html", 'r')
        except (IOError):
            raise MyError("Can't open template.html")
        self._template = jinja2.Template(TEMPLATE.read())
        TEMPLATE.close()
        
    def init_nb_cycles(self):
        Dict = read_csv(ROOT_MEASURE + '/app_cycles.csv')
        try:
            row = Dict.next()
            self._nb_cycles = row["CPU_CLK_UNHALTED_CORE"]
            self._nb_cycles = "{:e}".format(int(self._nb_cycles))
        except (StopIteration):
            raise MyError("/app_cycles.csv empty")
        except (KeyError):
            raise MyError("error key: not CPU_CLK_UNHALTED_CORE in /app_cycles.csv ")
        
    def init_regions(self):
        self._regions = {}
        match_error = read_csv(ROOT_MEASURE +"/matching_error.csv")
        for region in match_error:
            self._regions[suppr_prefix(region["Codelet Name"])] = Region(region)
        self.init_callcount()
        self.init_invocation_table()
        self.init_codes()
        
    def init_callcount(self):
        test = True
        i = 0
        while (test):
            try:
                loops = read_csv("{root}/level_{lev}.csv".format(
                                 root=ROOT_MEASURE,lev=i))
                i = i + 1
                for loop in loops:
                    try:
                        self._regions[suppr_prefix(loop["Codelet Name"])].set_callcount(loop["Call Count"])
                    except(KeyError):
                        if(DEBUG_MODE):
                            print "CALL_COUNT: " + suppr_prefix(loop["Codelet Name"]) + " not in matching error"
            except(MyError):
                test = False
        
    def init_invocation_table(self):
        invocations = read_csv(ROOT_MEASURE + '/invocations_error.csv')
        for inv in invocations:
            try:
                self._regions[suppr_prefix(inv["Codelet Name"])].append_invocation_table(inv)
            except(KeyError):
                if(DEBUG_MODE):
                    print "INVOCATIONS: " + suppr_prefix(inv["Codelet Name"]) + " not in matching error"
        
    def init_codes(self):
        try:
            FILE = open(NAME_FILE, 'rb')
        except (IOError):
            raise MyError("Can't read " + NAME_FILE + "-> Verify coverage and matching")
        table = csv.reader(FILE, delimiter=CSV_DELIMITER)
        for code_place in table:
            try:
                self._regions[suppr_prefix(code_place[0])].init_code(code_place)
            except(KeyError):
                if(DEBUG_MODE):
                    print "CODE_PLACE: " + suppr_prefix(code_place[0]) + " not in matching error"
        
    def init_liste_script(self):
        self._liste_script = []
        for region in self._regions:
            self._liste_script = self._liste_script + [self._regions[region]._code._script]
        
    def init_tree(self):
        self._tree = []
        tree = read_csv(ROOT_MEASURE + "/selected_codelets")
        for node in tree:
            try:
                region = self._regions[suppr_prefix(node["Codelet Name"])]
                region.init_selected(node["Selected"])
                self._tree = self._tree + [Node(node,region)]
            except(KeyError):
                if(DEBUG_MODE):
                    print "SELECTED_CODELETS: " + suppr_prefix(node["Codelet Name"]) + " not in matching error"
        self._liste_script = set(self._liste_script)
        self.test_parent_tree()
        
    def test_parent_tree(self):
        for node in self._tree:
            if (node._parent is not "none"):
                test = 0
                for parent in self._tree:
                    if(parent._id == node._parent):
                        test = 1
                        break
                if (test == 0):
                    node._parent = "none"
				

    def init_part(self):
        self._part = 0
        for region in self._regions:
            if(self._regions[region]._selected == "true"):
                self._part = self._part + self._regions[region]._table["Exec Time (%)"]
        
    def init_graph(self):
        self._graph_error = encode_graph("/bench_error.png")
        
    def init_javascript(self):
        try:
            with file(ROOT + '/Report.js') as jsf:
                self.javascript=jsf.read()
        except (IOError):
            raise MyError("Cannot find Report.js")
    
    def write_report(self):
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
    TEMP_DIR = os.getcwd()
    if(os.chdir(DIR)):
        exit("Error Report -> Can't find " + DIR)
    os.system(ROOT + "/../../src/granularity/graph_error.R")
    try:
        yield
    except MyError as err:
        exit("Error Report -> " + err.value)
    else:
        print ("Report created")
    os.chdir(TEMP_DIR)

def read_csv(File):
    try:
        FILE = open(File, 'rb')
    except (IOError):
        raise MyError("Can't read " + File + "-> Verify coverage and matching")
    Dict = csv.DictReader(FILE, delimiter=CSV_DELIMITER)
    return Dict


def encode_graph(graph_name):
        try:
            with open(ROOT_GRAPHS + graph_name, "rb") as f:
                graph = f.read()
        except (IOError):
            raise MyError("Cannot find " + graph_name)
        return base64.standard_b64encode(graph)


def percent(x):
    return (100 * float(x))


def suppr_prefix(name):
    for pre in LIST_PREFIX:
        name = name.replace(pre,"")
    return name


def read_code(code_place):
    EXT = "__None__"
    for ext in EXTENSIONS:
        try:
            FILE = open(code_place[2]+ext, "r")
            code = FILE.readlines()
            code = "".join(code)
            try:
                code.encode('utf-8')
            except (UnicodeDecodeError):
                return Code(".html", "ERROR UNICODE -> FILE:" +code_place[2]+ext , 1)
            FILE.close()
            EXT = ext
        except (IOError):
            pass
    if (EXT == "__None__"):
        raise MyError("Can't open: "+code_place[2])
    return Code(EXT, code, code_place[4])


def main():
    '''
    Main function
    '''
    parser = argparse.ArgumentParser()
    parser.add_argument('dir')
    args = parser.parse_args()
    with context(args.dir):
        bench = os.getcwd().split("/") [-1]
        REPORT = Report(bench)
        REPORT.write_report()


if __name__ == "__main__":
    main()
