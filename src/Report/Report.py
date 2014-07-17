#!/usr/bin/env python
# Loop Extractor Compiler
# (C) 2013 University of Versailles
import os
import argparse
import jinja2
import csv
import base64
from contextlib import contextmanager
EXTENSIONS = [".c",".f",".f90",".C",".F",".F90"]
Mode_dict = {".c":["clike/clike.js","text/x-csrc"], ".C":["clike/clike.js",
             "text/x-csrc"], ".f":["fortran/fortran.js", "text/x-Fortran"],
             ".F":["fortran/fortran.js", "text/x-Fortran"],
             ".f90":["fortran/fortran.js", "text/x-Fortran"],
             ".F90":["fortran/fortran.js", "text/x-Fortran"],
             ".html":["htmlmixed/htmlmixed.js", "text/htmlmixed"]}
ROOT = os.path.dirname(os.path.realpath(__file__))
ROOT_MEASURE = "./measures"
ROOT_GRAPHS = "./measures/plots"
NAME_FILE = "regions.csv"
CSV_DELIMITER = ','
REGIONS_FIELDNAMES = ["Exec Time (%)", "Codelet Name", "Error (%)"]
INVOCATION_FIELDNAMES = ["Invocation", "Cluster", "Part", "Invitro (cycles)",
                         "Invivo (cycles)", "Error (%)"]


class MyError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)


class Code:
    def __init__(self, Id, ext, value, line):
        self._id = Id
        self._value = value
        self._line = line
        self._mode = Mode_dict[ext][1]
        self._script = Mode_dict[ext][0]
    def getScript(self):
        return self._script


class Dict_region():
    '''
    We read in the different level csv to obtain all region with call count
    '''
    def __init__(self):
        self.dict = {}
        test = True
        i = 0
        while (test):
            try:
                loops = read_csv("{root}/level_{lev}.csv".format(
                                 root=ROOT_MEASURE,lev=i))
                i = i + 1
                for loop in loops:
                    self.dict[(loop["Codelet Name"],"callcount")] = loop["Call Count"]
            except(MyError):
                test = False


class table_region():
    def __init__(self):
        try:
            FILE = open(NAME_FILE, 'rb')
        except (IOError):
            raise MyError("Can't read " + NAME_FILE + "-> Verify coverage and matching")
        table = csv.reader(FILE, delimiter=CSV_DELIMITER)
        self._table = []
        for region in table:
            self._table = self._table + [region]
        for region in self._table:
            region[0] = rm_prefix(region[0])


class Report:
    def __init__(self,bench):
        self._bench = bench
        self.init_template()
        self.init_nb_cycles()
        self.init_regions()
        self.init_invocations()
        self.init_codes()
        self.init_liste_script()
        self.init_part()
        self.init_graphs()
        
    def init_template(self):
        try:
            TEMPLATE=open(ROOT + "/template.html", 'r')
        except (IOError):
            raise MyError("Can't open template.html")
        self._template = jinja2.Template(TEMPLATE.read())
        TEMPLATE.close()
        
    def init_nb_cycles(self):
        Dict = read_csv(ROOT_MEASURE + '/app_cycles.csv')
        row = Dict.next()
        self._nb_cycles = row["CPU_CLK_UNHALTED_CORE"]
        self._nb_cycles = "{:e}".format(int(self._nb_cycles))
        
    def init_regions(self):
        
        self._regions = read_csv(ROOT_MEASURE +"/matching_error.csv")
        regions = read_csv(ROOT_MEASURE + "/selected_codelets")
        temp = []
        temp_2 = []
        for region in self._regions:
            temp_2 = temp_2 + [region]
        for region in regions:
            for region_2 in temp_2:
                if (region["Codelet Name"] == region_2["Codelet Name"]):
                    region_2 = dict(region.items() + region_2.items())
                    temp = temp + [region_2]
        self._regions = temp
        self._regions = map(rewrite_dict_region, self._regions)
        
    def init_invocations(self):
        self._invocations = read_csv(ROOT_MEASURE + '/invocations_error.csv')
        self._invocations = map(rewrite_dict_invocation, self._invocations)
        
    def init_codes(self):
        self._codes = map(read_code, self._regions)
        
    def init_liste_script(self):
        self._liste_script = []
        for code in self._codes:
            self._liste_script = self._liste_script + [code._script]
        self._liste_script = set(self._liste_script)
        
    def init_part(self):
        self._part = 0
        for region in self._regions:
            if((region["Error (%)"] < 15) and ( region["selected"] == "true")):
                self._part = self._part + region["Exec Time (%)"]
        
    def init_graphs(self):
        self._graph_error = encode_graph("/bench_error.png")
        self._graphs = []
        for region in self._regions:
            graph_invoc = encode_graph("/{region}.png".format(region=region["Codelet Name"]))
            graph_clustering = encode_graph("/{region}_byPhase.png".format(region=region["Codelet Name"]))
            self._graphs = self._graphs +[{"id":region["id"],
                                           "graph_invoc":graph_invoc,
                                           "graph_clustering":graph_clustering}]
    
    def write_report(self):
        try:
            REPORT=open(self._bench+'.html','w')
        except (IOError):
            raise MyError("Cannot open "+ self._bench +".html")
        try:
            with file(ROOT + '/Report.js') as jsf:
                REPORT_JS=jsf.read()
        except (IOError):
            raise MyError("Cannot find Report.js")

        REPORT.write(self._template.render(bench=self._bench, root=ROOT, nb_cycles=self._nb_cycles,
                    regionlist=self._regions, regionfields=REGIONS_FIELDNAMES,
                    invocationlist=self._invocations, invocationfields=INVOCATION_FIELDNAMES,
                    codes=self._codes, l_modes=self._liste_script, report_js=REPORT_JS, part=self._part,
                    graph_error=self._graph_error,graphs=self._graphs))
        REPORT.close()


@contextmanager
def context(DIR):
    TEMP_DIR = os.getcwd()
    if(os.chdir(DIR)):
        exit("Error Report -> Can't find " + DIR)
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


def rm_prefix(name):
    name = name.split("__")
    name = name[2:]
    return "__".join(name)


DICT = Dict_region()
TABLE = table_region()


def encode_graph(graph_name):
        try:
            with open(ROOT_GRAPHS + graph_name, "rb") as f:
                graph = f.read()
        except (IOError):
            raise MyError("Cannot find " + graph_name)
        return base64.b64encode(graph)


def percent(x):
    return (100 * float(x))


def rewrite_dict_region(x):
    Dict = {"Exec Time (%)":percent(x["Exec Time"]), "Codelet Name":x["Codelet Name"],
            "Error (%)":percent(x["Error"]), "nb_invoc":DICT.dict[(x["Codelet Name"],"callcount")],
            "Invivo":"{:e}".format(float(x["Invivo"])), "Invitro":"{:e}".format(float(x["Invitro"])),
            "parent":x["ParentId"], "id":x["Id"], "selected":x["Selected"]}
    return Dict


def rewrite_dict_invocation(x):
    Dict = {"Cluster":x["Cluster"], "Invocation":x["Invocation"], "Part":x["Part"],
            "Codelet Name":x["Codelet Name"], "Invivo (cycles)":"{:e}".format(float(x["Invivo"])),
            "Invitro (cycles)":"{:e}".format(float(x["Invitro"])), "Error (%)":percent(x["Error"])}
    return Dict


def obtain_name(region):
    region = rm_prefix(region)
    for loop in TABLE._table:
        if (loop[0] == region):
            return [loop[0], loop[2], loop[3], loop[4]]


def read_code(region):
    temp = obtain_name(region["Codelet Name"])
    EXT = "__None__"
    for ext in EXTENSIONS:
        try:
            FILE = open(temp[1]+ext, "r")
            code = FILE.readlines()
            code = "".join(code)
            FILE.close()
            EXT = ext
        except (IOError):
            pass
    if (EXT == "__None__"):
        raise MyError("Can't open: "+temp[1])
    return Code(region["id"], EXT, code, temp[3])


def main():
    '''
    Main function
    '''
    try:
        os.system(ROOT+"/../granularity/graph_error.R")
    except (IOError):
        exit("Error Report -> Can't find " + DIR)
    parser = argparse.ArgumentParser()
    parser.add_argument('dir')
    args = parser.parse_args()
    with context(args.dir):
        bench = os.getcwd().split("/") [-1]
        REPORT = Report(bench)
        REPORT.write_report()


if __name__ == "__main__":
    main()
