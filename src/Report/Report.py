#!/usr/bin/env python
# Loop Extractor Compiler
# (C) 2013 University of Versailles
import os
import argparse
import jinja2
import csv
EXTENSIONS = [".c",".f",".f90",".C",".F",".F90"]
Dict = {".c":["clike/clike.js","text/x-csrc"],".C":["clike/clike.js",
        "text/x-csrc"],".f":["fortran/fortran.js","text/x-Fortran"],
        ".F":["fortran/fortran.js","text/x-Fortran"],".f90":["fortran/fortran.js",
        "text/x-Fortran"],".F90":["fortran/fortran.js","text/x-Fortran"]}
Unit = {0 : "", 3 :"Kilo", 6 : "Mega", 9 : "Giga", 12 : "Terra"}
ROOT = os.path.dirname(os.path.realpath(__file__))
ROOT_MEASURE = "./measures"
CSV_DELIMITER = ','
COLUMN_NAME = "Codelet Name"
TEMP_DIR = ""


def popd():
    global TEMP_DIR
    os.chdir(TEMP_DIR)
    TEMP_DIR = ""


def safe_exit(error):
    if (TEMP_DIR):
        popd()
    exit("Error Report -> " + error)


def pushd(DIR):
    global TEMP_DIR
    TEMP_DIR = os.getcwd()
    if(os.chdir(DIR)):
        safe_exit("Can't find " + DIR)


def get_template():
    TEMPLATE=open(ROOT + '/template.html', 'r')
    if (not TEMPLATE):
        safe_exit("Can't open template.html")
    template = jinja2.Template(TEMPLATE.read())
    TEMPLATE.close()
    return template


def write_report(template, BENCH, nb_cycles, REGIONS, REGIONS_FIELDNAMES,
                 CODES, LISTE_MODES, INVOCATIONS, INVOCATION_FIELDNAMES):
    REPORT=open(BENCH+'.html','w')
    if (not REPORT):
        safe_exit("Can't open test.html")
    REPORT.write(template.render(bench=BENCH, root=ROOT, nb_cycles=nb_cycles,
                 regionlist=REGIONS, regionfields=REGIONS_FIELDNAMES,
                 invocationlist=INVOCATIONS, invocationfields=INVOCATION_FIELDNAMES,
                 codes=CODES, l_modes=LISTE_MODES))
    REPORT.close()


def read_csv(File):
    FILE = open(File, 'rb')
    if (not FILE):
        safe_exit("Can't read " + File)
    Dict = csv.DictReader(FILE, delimiter=CSV_DELIMITER)
    return Dict


def get_nb_cycles():
    Dict = read_csv(ROOT_MEASURE + '/app_cycles.csv')
    row = Dict.next()
    nb_cycles = row["CPU_CLK_UNHALTED_CORE"]
    return nb_cycles


def convert(nb_cycles):
    temp = int(float(nb_cycles))
    i = 0
    while (temp/1000):
        temp = temp/1000
        i = i+3
    word = ""
    for j,letter in enumerate(nb_cycles):
        word = word + letter
        if (j == len(nb_cycles) - i - 1):
            word = word + ','
    temp = word + " " + Unit[i]
    return temp


def Percent(x):
    return (100 * float(x))


def Convert_Percent_region(x):
    Dict = {"Exec Time (%)":Percent(x["Exec Time"]), COLUMN_NAME:x[COLUMN_NAME],
            "Error (%)":Percent(x["Error"])}
    return Dict


def Convert_Percent_invocation(x):
    Dict = {"Cluster":x["Cluster"], "Invocation":x["Invocation"], 
            "Part":x["Part"], COLUMN_NAME:x[COLUMN_NAME], "Invivo (cycles)":convert(x["Invivo"]),
            "Invitro (cycles)":convert(x["Invitro"]), "Error (%)":Percent(x["Error"])}
    return Dict


def sort_regions_fieldnames(regions_fieldnames):
    return[regions_fieldnames[2] + " (%)", regions_fieldnames[0],
           regions_fieldnames[1] + " (%)"]


def get_tab(FILE):
    Dict = read_csv(ROOT_MEASURE + FILE)
    FIELDNAMES = Dict.fieldnames
    return Dict, FIELDNAMES


def get_tab_regions():
    REGIONS, REGIONS_FIELDNAMES = get_tab('/matching_error.csv')
    REGIONS = (i[1] for i in reversed(sorted(enumerate(REGIONS), key=lambda x:x[1]["Exec Time"])))
    REGIONS = map(Convert_Percent_region, REGIONS)
    REGIONS_FIELDNAMES = sort_regions_fieldnames(REGIONS_FIELDNAMES)
    return REGIONS, REGIONS_FIELDNAMES


def get_tab_invocation():
    INVOCATIONS, INVOCATIONS_FIELDNAMES = get_tab('/invocations_error.csv')
    INVOCATIONS = map(Convert_Percent_invocation, INVOCATIONS)
    INVOCATIONS_FIELDNAMES[-1] = INVOCATIONS_FIELDNAMES[-1] + " (%)"
    INVOCATIONS_FIELDNAMES[-2] = INVOCATIONS_FIELDNAMES[-2] + " (cycles)"
    INVOCATIONS_FIELDNAMES[-3] = INVOCATIONS_FIELDNAMES[-3] + " (cycles)"
    return INVOCATIONS, INVOCATIONS_FIELDNAMES


def sep_name(x):
    region = x[COLUMN_NAME]
    temp = region
    filename = ""
    functionName = ""
    line = ""
    i = 0
    while (i < 4):
        if (temp[0] == "_"):
            i = i + 1
        temp = temp[1:]
    while i < 5:
        if (temp[-1] == "_"):
            i = i + 1
        else:
            line = temp[-1] + line
        temp = temp[:-1]
    for letters in temp:
        if (letters == "_"):
            i = i + 1
        else:
            if(i > 5):
                functionName = functionName + letters
            else:
                filename = filename + letters
    if (i > 6):
        functionName = "Erreur"
    return [region, filename, functionName, line]


def read_code(x):
    temp = sep_name(x)
    if (temp[2] == "Erreur"):
        return [temp[0], "Can't obtain source code -> number of '_' > 6",
                "htmlmixed/htmlmixed.js", "text/htmlmixed", 1]
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
        safe_exit("Can't open: "+temp[1])
    return ([temp[0], code]+Dict[EXT]+[temp[3]])


def obtains_code(REGIONS):
    CODES = map(read_code, REGIONS)
    return CODES


def obtains_liste_mode(codes):
    liste = []
    for code in codes:
        liste = liste + [code[2]]
        code.pop(2)
    liste = set(liste)
    return liste, codes


def main():
    '''
    Main function
    '''
    parser = argparse.ArgumentParser()
    parser.add_argument('dir')
    args = parser.parse_args()
    pushd(args.dir)
    BENCH = (args.dir.split("/"))[-2]
    nb_cycles = get_nb_cycles()
    template = get_template()
    REGIONS, REGIONS_FIELDNAMES = get_tab_regions()
    INVOCATIONS, INVOCATION_FIELDNAMES = get_tab_invocation()
    CODES = obtains_code(REGIONS)
    LISTE_MODES, CODES = obtains_liste_mode(CODES)
    nb_cycles = convert(nb_cycles)
    write_report(template, BENCH, nb_cycles, REGIONS, REGIONS_FIELDNAMES,
                 CODES, LISTE_MODES, INVOCATIONS, INVOCATION_FIELDNAMES)
    popd()


if __name__ == "__main__":
    main()
