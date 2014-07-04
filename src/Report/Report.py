#!/usr/bin/env python
# Loop Extractor Compiler
# (C) 2013 University of Versailles
import os
import argparse
import jinja2
import csv
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
    def __init__(self, name, ext, value, line):
        self._name = name
        self._value = value
        self._line = line
        self._mode = Mode_dict[ext][1]
        self._script = Mode_dict[ext][1]
    def getScript(self):
        return self._script


class Dict_call_count():
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
                    self.dict[loop["Codelet Name"]] = loop["Call Count"]
            except(MyError):
                test = False


class Report:
    def __init__(self,bench):
        self._bench = bench
        self.init_template()
        self.init_nb_cycles()
        self.init_regions()
        self.init_invocations()
        self.init_codes()
        self.init_liste_script()
        
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
        self._regions = (i[1] for i in reversed(sorted(enumerate(self._regions),
                         key=lambda x:x[1]["Exec Time"])))
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
        
    def write_report(self):
        try:
            REPORT=open(self._bench+'.html','w')
        except (IOError):
            raise MyError("Can't open "+ self._bench +".html")
        REPORT.write(self._template.render(bench=self._bench, root=ROOT, nb_cycles=self._nb_cycles,
                    regionlist=self._regions, regionfields=REGIONS_FIELDNAMES,
                    invocationlist=self._invocations, invocationfields=INVOCATION_FIELDNAMES,
                    codes=self._codes, l_modes=self._liste_script))
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
        raise MyError("Can't read " + File + "\nVerify coverage and matching")
    Dict = csv.DictReader(FILE, delimiter=CSV_DELIMITER)
    return Dict


dict_call_count = Dict_call_count()


def percent(x):
    return (100 * float(x))


def rewrite_dict_region(x):
    Dict = {"Exec Time (%)":percent(x["Exec Time"]), "Codelet Name":x["Codelet Name"],
            "Error (%)":percent(x["Error"]), "nb_invoc":dict_call_count.dict[x["Codelet Name"]]}
    return Dict


def rewrite_dict_invocation(x):
    Dict = {"Cluster":x["Cluster"], "Invocation":x["Invocation"], "Part":x["Part"],
            "Codelet Name":x["Codelet Name"], "Invivo (cycles)":"{:e}".format(float(x["Invivo"])),
            "Invitro (cycles)":"{:e}".format(float(x["Invitro"])), "Error (%)":percent(x["Error"])}
    return Dict


def sep_name(region):
    '''
    Separate a region name -> __prefixe__filename_functionname_line
    in filename functionname and line
    '''
    temp = region.split('_')
    filename = temp[4]
    functionName = temp[5]
    line = temp[-1]
    #if the name is not a wanted name
    if (len(temp) > 7):
        functionName = "Erreur"
    return [region, filename, functionName, line]


def read_code(region):
    temp = sep_name(region["Codelet Name"])
    if (temp[2] == "Erreur"):
        return Code(temp[0], ".html", "Can't obtain source code -> Codelet Name can't "+
                    "separated in __prefixe__filename_functionname_line", 1)
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
    return Code(temp[0], EXT, code, temp[3])


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
