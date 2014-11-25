#!/usr/bin/env python
# Loop Extractor Compiler
# (C) 2013 University of Versailles
import os
import argparse
import jinja2
import csv
import base64
from contextlib import contextmanager
EXTENSIONS = [".c",".f",".f90",".C",".F",".F90",".cc",".cpp"]
Mode_dict = {".c":["clike/clike.js","text/x-csrc"], ".C":["clike/clike.js",
             "text/x-csrc"], ".f":["fortran/fortran.js", "text/x-Fortran"],
             ".F":["fortran/fortran.js", "text/x-fortran"],
             ".f90":["fortran/fortran.js", "text/x-fortran"],
             ".F90":["fortran/fortran.js", "text/x-fortran"],
             ".html":["htmlmixed/htmlmixed.js", "text/htmlmixed"],
             ".cc":["clike/clike.js", "text/x-c++src"],
             ".cpp":["clike/clike.js", "text/x-c++src"]}
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
    def __init__(self,region):
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
        self._name = region["Codelet Name"]
        self._invivo = "{:e}".format(float(region["Invivo"]))
        self._invitro = "{:e}".format(float(region["Invitro"]))
        self._table = {"Exec Time (%)":percent(region["Exec Time"]), "Error (%)":percent(region["Error"]),
                       "Codelet Name":suppr_prefix(region["Codelet Name"])}
        self._inv_table = []
        self._code = Code(".html", "CODE NOT FOUND -> THIS CODELET NOT IN regions.csv?", 1)
        self._callcount = 0
        self._selected = "false"
        self._tooSmall = "false"
        self._execError = "false"
        self.init_graph()
    
    def init_graph(self):
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
    def __init__(self,bench,mincycles):
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
        self.init_regions(mincycles)
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
        except (IOError):
            raise MyError("Can't open template.html")
        self._template = jinja2.Template(TEMPLATE.read())
        TEMPLATE.close()
        
    def init_nb_cycles(self):
        '''
        Read the nb_cycles value of application in app_cycles.csv
        '''
        Dict = read_csv(ROOT_MEASURE + '/app_cycles.csv')
        try:
            row = Dict.next()
            self._nb_cycles = row["CPU_CLK_UNHALTED_CORE"]
            self._nb_cycles = "{:e}".format(int(self._nb_cycles))
        except (StopIteration):
            raise MyError("/app_cycles.csv empty")
        except (KeyError):
            raise MyError("error key: not CPU_CLK_UNHALTED_CORE in /app_cycles.csv ")
        
    def init_regions(self,mincycles):
        '''
        Initialize the region list
        For each region in matching_error we create a new objet Region, which contains all information neccessary 
        for the report about this region
        Then we initialize the value not in matching_error.csv like callcount
        '''
        self._regions = {}
        match_error = read_csv(ROOT_MEASURE +"/matching_error.csv")
        for region in match_error:
            self._regions[suppr_prefix(region["Codelet Name"])] = Region(region)
        self.init_callcount()
        for k,r in self._regions.iteritems():
            if r._table["Error (%)"] == 100:
                self._regions[k]._execError = "true"
        self.init_invocation_table()
        self.init_codes()
        
    def init_callcount(self):
        '''
        Initialize the callcount
        We read all files level_*.csv and for each region in each file we initialize his callcount with the value in the file
        '''
        for k,r in self._regions.iteritems():
            infos = csv.reader(open(ROOT_MEASURE+"/__invivo__{0}.csv".format(k)))
            line = infos.next()
            line = infos.next()
            try:
                self._regions[k].set_callcount(line[1])
            except(KeyError):
                if(DEBUG_MODE):
                    print "CALL_COUNT: " + suppr_prefix(loop["Codelet Name"]) + " not in matching error"
        
    def init_invocation_table(self):
        '''
        Initialize the information about the different invocation of regions
        We read invocations_error and append invocation information for the region in column "Codelet Name"
        '''
        invocations = read_csv(ROOT_MEASURE + '/invocations_error.csv')
        for inv in invocations:
            try:
                self._regions[suppr_prefix(inv["Codelet Name"])].append_invocation_table(inv)
            except(KeyError):
                if(DEBUG_MODE):
                    print "INVOCATIONS: " + suppr_prefix(inv["Codelet Name"]) + " not in matching error"
        
    def init_codes(self):
        '''
        Initialize the list of Code.
        For each region we extract code in file to print them in Report
        '''
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
        tree = read_csv(ROOT_MEASURE + "/selected_codelets")
        for node in tree:
            try:
                region = self._regions[suppr_prefix(node["Codelet Name"])]
                region.init_selected(node["Selected"])
                self._tree = self._tree + [Node(node,region)]
            except(KeyError):
                if(DEBUG_MODE):
                    print "SELECTED_CODELETS: " + suppr_prefix(node["Codelet Name"]) + " not in matching error"
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
    '''
    Read graph and encode this graph in base 64.
    '''
    try:
        with open(ROOT_GRAPHS + graph_name, "rb") as f:
            graph = f.read()
    except (IOError):
        raise MyError("Cannot find " + graph_name)
    return base64.standard_b64encode(graph)


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
    Change directory to the directory passed by the script call
    Then obtain bench name by looking at the last element of folder path
    We initialize the Report object and write him directly in the folder 
    '''
    parser = argparse.ArgumentParser()
    parser.add_argument('dir')
    parser.add_argument('--mincycles', type=int,default=0)
    args = parser.parse_args()
    with context(args.dir):
        bench = os.getcwd().split("/") [-1]
        REPORT = Report(bench,args.mincycles)
        REPORT.write_report()


if __name__ == "__main__":
    main()
