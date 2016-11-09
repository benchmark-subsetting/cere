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
import csv

from cere.vars import *

OMP_FLAGS=""
if "CERE_OMP" in os.environ:
    OMP_FLAGS="-omp"
BACKEND_FLAGS=""
if "CERE_BACKEND_FLAGS" in os.environ:
    BACKEND_FLAGS=os.environ["CERE_BACKEND_FLAGS"]
MIDEND_FLAGS=""
if "CERE_MIDEND_FLAGS" in os.environ:
    MIDEND_FLAGS=os.environ["CERE_MIDEND_FLAGS"]
BACKEND_USE=""
if "CERE_LLC" in os.environ:
    BACKEND_USE="llc"
REGION_EXTRACTED = False

def fail_lec(error_message):
    exit("Error {prog} : {cmd}".format(prog='lec', cmd=error_message))

def safe_system(command, EXIT=True):
    '''
    Try-catch system call
    Verify system call and exit with appropriate error message
    '''
    if(os.system(command)):
        if (EXIT):
            fail_lec("safe_system -> " + command)
        else:
            print("Warning Error {prog} : safe_system -> {cmd}".format(
                  prog='lec', cmd=command))

#Keep regions that are in sources
def read_file(regions_file, regions_infos, sources):
  regions = {}
  all_regions = {}

  if not (os.path.isfile(regions_infos)):
    fail_lec("No such file: {0}".format(regions_infos))
  with open(regions_infos) as regions_list:
    regions_reader = csv.DictReader(regions_list)
    for regions_row in regions_reader:
      all_regions[regions_row["Region Name"]] = {"file":os.path.basename(regions_row["Original Location"]), "function": regions_row["Function Name"]}
  with open(regions_file) as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
      if (row["Region"] in all_regions) and (all_regions[row["Region"]]["file"] in sources) and (int(row["Id"]) >= 0):
        regions[row["Region"]] = {"midend": row["Mid_end"], "backend": row["Back_end"], "file": all_regions[row["Region"]]["file"], "function": all_regions[row["Region"]]["function"]}
  return regions

def dump_fun(mode_opt, BASE, regions):
    '''
    Dump mode
    Call LoopExtractor and LoopManager in dump mode on wanted loops
    by passing options like --invocation
    In link mode: Call the linker and copy the original binary
    '''
    temp = ""
    print "Compiling dump mode"
    if(mode_opt.region):
        temp = " --region="+mode_opt.region+" "
        if(mode_opt.regions_file):
            fail_lec("--regions-file and --region are incompatible")
    else:
        if(mode_opt.regions_file):
            temp = temp+"--regions-file="+mode_opt.regions_file+" "
    if(mode_opt.invocation):
        temp = temp+"--invocation="+mode_opt.invocation+" "
    safe_system(("{llvm_bindir}/opt -S -load {LoopExt} {Omp}-region-outliner " +
                "{base}.ll -o {base}.ll").format(llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT,
                LoopExt=LOOP_EXT, base=BASE,Omp=OMP_FLAGS), EXIT=False)
    safe_system(("{llvm_bindir}/opt -S -load {LoopMan} {Omp}-region-dump {opts} " +
                "{base}.ll -o {base}.ll").format(llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT,
                LoopMan=LOOP_DUMP, opts=temp, base=BASE,Omp=OMP_FLAGS), EXIT=False)

    globalize_variables(BASE, mode_opt)

#in replay mode
def replay_fun(mode_opt, BASE, regions):
    '''
    Replay mode
    Call LoopExtractor and LoopManager in replay mode on wanted loop
    by passing options like --invocation
    '''
    temp = ""
    if (not (mode_opt.region)):
        fail_lec("Need --region with replay mode")
    print "Compiling replay mode"
    if (mode_opt.invocation):
        temp = temp + "--invocation=" + mode_opt.invocation + " "
    safe_system(("{llvm_bindir}/opt -S -load {LoopExt} {Omp}-region-outliner " +
                "-isolate-region={loop} {base}.ll -o {base}.ll").format(
                llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT, LoopExt=LOOP_EXT,
                loop=mode_opt.region, base=BASE,Omp=OMP_FLAGS), EXIT=False)
    safe_system(("{llvm_bindir}/opt -S -load {LoopMan} {opts} {Omp}-region-replay -region={loop} " +
                "{base}.ll -o {base}.ll").format(llvm_bindir=LLVM_BINDIR,
                    Root=PROJECT_ROOT, LoopMan=LOOP_REPLAY, opts=temp, loop=mode_opt.region,
                    base=BASE,Omp=OMP_FLAGS), EXIT=False)
    if (mode_opt.instrument):
        print "Instrumentation mode"
        temp_instr = "--instrument-region=" + mode_opt.region + " "
        safe_system(("{llvm_bindir}/opt -S -loop-simplify {base}.ll -o {base}.ll").format(
                    llvm_bindir=LLVM_BINDIR,
                    base=BASE), EXIT=False)
        safe_system(("{llvm_bindir}/opt -S -load {LoopInstr} " +
                    "{Omp}-region-instrumentation --replay {opts} {base}.ll " +
                    "-o {base}.ll").format(
                    llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT,
                    LoopInstr=LOOP_INSTR, opts=temp_instr, base=BASE,Omp=OMP_FLAGS),
                    EXIT=False)

    globalize_variables(BASE, mode_opt)

def globalize_variables(BASE, mode_opt):
    # When inside the compilation unit that contains the replayable
    # code internal symbols should be globalized
    # Globalizing symbols ensures that symbol is not bound until link time.
    # At link time, symbols will be weaken and replaced by the ones captured
    # during dump that are exported in the dump .sym file.

    # Globalization is also done during dump to ensure that the variable is not
    # optimized out during capture (see /issues/51)

    # XXX: An LLVM pass would be preferable than the ugly sed hack below.
    if BASE in mode_opt.region:
       os.system("sed -i 's/internal global/global/' {base}.ll".format(base=BASE))

#Extract region into a seperate file
def extract_function(mode_opt, regions, BASE):
  for region, data in regions.items():
        baseName = os.path.splitext(data["file"])[0]
        if baseName == BASE:
          to_extract=data["function"]
          #Extract at loop lvl
          if mode_opt.extraction_lvl == "loop":
            to_extract=region
            #Outline the loop into a function
            safe_system(("{llvm_bindir}/opt -S -load {LoopExt} {Omp}-region-outliner " +
                 "-isolate-region={loop} {base}.ll -o {base}.ll").format(llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT,
                 LoopExt=LOOP_EXT, loop=to_extract, base=BASE, Omp=OMP_FLAGS), EXIT=False)

          #Rename global variables in this module
          safe_system(("{llvm_bindir}/opt -S -load {globRename} -global-rename " +
                    "{base}.ll -o {base}.ll").format(llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT,
                    globRename=GLOB_RENAME, base=BASE), EXIT=False)
          #Then extract this function into a new file
          safe_system("llvm-extract -S -func={0} {1}.ll -o {0}.ll".format(to_extract, baseName), EXIT=False)
          safe_system("llvm-extract -S -func={0} {1}.ll -o {1}.ll -delete".format(to_extract, baseName), EXIT=False)

          global REGION_EXTRACTED
          REGION_EXTRACTED = True

#in original mode
def original_fun(mode_opt, BASE, regions):
    '''
    Original mode
    Call LoopInstrumentation on wanted loops
    by passing options like --invocation
    '''
    extract_function(mode_opt, regions, BASE)

    instru_opts = ""
    extract_opts = ""

    if(mode_opt.region):
        instru_opts = instru_opts + "--instrument-region=" + mode_opt.region + " "
        if(mode_opt.regions_file):
            fail_lec("--regions-file and --region are incompatible")
        if(mode_opt.invocation):
            instru_opts = instru_opts + "--invocation=" + mode_opt.invocation + " "
        if(mode_opt.instrument_app):
            fail_lec("--instrument-app and --region are incompatible")
    else:
        if(mode_opt.invocation):
            fail_lec("Can't measure specific invocation with --regions-file")
        if(mode_opt.regions_file):
            instru_opts = instru_opts + "--regions-file=" + mode_opt.regions_file + " "
            if(mode_opt.instrument_app):
                fail_lec("--regions-file and --instrument-app are incompatible")
            if(mode_opt.invocation):
                fail_lec("--regions-file and --invocation are incompatible")
        else:
            if(mode_opt.instrument_app):
                extract_opts = extract_opts + "--instrument-app "
            else:
                instru_opts = instru_opts + "--instrument-app "
    if(mode_opt.instrument):
        if(mode_opt.instrument_app):
          if(mode_opt.regions_infos):
            extract_opts = extract_opts + "-regions-infos=" + mode_opt.regions_infos + " "
          safe_system(("{llvm_bindir}/opt -S -load {LoopExt} {Omp}-region-outliner {opts} " +
                      "{base}.ll -o {base}.ll").format(llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT,
                      LoopExt=LOOP_EXT, opts=extract_opts, base=BASE,Omp=OMP_FLAGS), EXIT=False)
        else:
            safe_system(("{llvm_bindir}/opt -S -load {LoopInstr} " +
                        "{Omp}-region-instrumentation {opts} {base}.ll " +
                        "-o {base}.ll").format(
                        llvm_bindir=LLVM_BINDIR, Root=PROJECT_ROOT,
                        LoopInstr=LOOP_INSTR, opts=instru_opts, base=BASE,Omp=OMP_FLAGS, EXIT=False))

def first_compil(INCLUDES, SOURCE, BASE, ext, COMPIL_OPT):
    '''
    First Compilation
    Detect source language (fortran or C/C++ for the moment)
    and compile SOURCE code
    '''

    if ext in FORTRAN_EXTENSIONS:
        if DRAGONEGG_PATH:
            opt = [s for s in COMPIL_OPT if s.startswith('-J')]
            if opt:
              INCLUDES.append(opt[0])
            safe_system(("{gcc} -O0 -g {includes} -cpp {source} -S " +
                        "-fplugin={dragonegg} -fplugin-arg-dragonegg-emit-ir -o {base}.ll").format(
                        gcc=GCC, opts=" ".join(COMPIL_OPT), includes=" ".join(INCLUDES), source=SOURCE,
                        Root=PROJECT_ROOT, dragonegg=DRAGONEGG_PATH, base=BASE))
        else:
            fail_lec("fortran support disabled. Please reconfigure using --with-dragonegg.")
    else:
        safe_system(("{llvm_bindir}/clang {opts} -O0 -g {includes} {source} -S -emit-llvm -o " +
                    "{base}.ll").format(llvm_bindir=LLVM_BINDIR, opts=" ".join(COMPIL_OPT), includes=" ".join(INCLUDES),
                    source=SOURCE, base=BASE))

def last_compil(INCLUDES, SOURCE, BASE, OBJECT, COMPIL_OPT):
    '''
    Last Compilation
    Compile BASE.ll
    If Error compile with INCLUDES
    '''
    # Optionnal midend optimizations to explore
    if (MIDEND_FLAGS):
        os.system("{llvm_bindir}/opt -S {midend_flags} {base}.ll -o {base}.ll".format(
                     llvm_bindir=LLVM_BINDIR, midend_flags=MIDEND_FLAGS, base=BASE))

    if REGION_EXTRACTED:
        #Regions have been extracted from this file. So we must change globals
        #definitions.
        safe_system("opt -S -O3 {base}.ll -o {base}.ll".format(opts=" ".join(COMPIL_OPT),
                                                               base=BASE))
        os.system("sed -i 's/internal constant/hidden constant/' {base}.ll".format(base=BASE))
        #This prevent from new globals optimizations
        os.system("llc -O3 {base}.ll -o {base}.s".format(opts=" ".join(COMPIL_OPT), base=BASE))
        if "-g" in COMPIL_OPT:
            COMPIL_OPT = [x for x in COMPIL_OPT if x != "-g"]
        safe_system("clang -c {opts} {base}.s -o {object}".format(opts=" ".join(COMPIL_OPT),
                                                                  base=BASE, object=OBJECT))
    else:
        # Can choose llc as llvm backend
        failure = False
        if (BACKEND_USE):
            failure = os.system("{llvm_bindir}/{backend} -filetype=obj {backend_flags} {base}.ll -o {object}".format(
                llvm_bindir=LLVM_BINDIR, backend=BACKEND_USE,
                backend_flags=BACKEND_FLAGS, base=BASE, object=OBJECT))
        else:
            failure = os.system("{llvm_bindir}/clang -c {opts} {backend_flags} {base}.ll -o {object}".format(
                llvm_bindir=LLVM_BINDIR, opts=" ".join(COMPIL_OPT),
                backend_flags=BACKEND_FLAGS, base=BASE, object=OBJECT))

        # In case of failure fallback to a simple clang compilation without any CERE passes
        if (failure):
            safe_system("{llvm_bindir}/clang -c {opts} {backend_flags} {includes} {source} -o {object}".format(
                llvm_bindir=LLVM_BINDIR, opts=" ".join(COMPIL_OPT), includes=" ".join(INCLUDES),
                backend_flags=BACKEND_FLAGS, source=SOURCE, base=BASE, object=OBJECT))

def compile(args, args2):
    function={}
    function["replay_fun"] = replay_fun
    function["dump_fun"] = dump_fun
    function["original_fun"] = original_fun

    SOURCES = []
    if (len(args2[1]) == 0):
        exit("Error:Need source file")
    INCLUDES = args2[0].Inc
    COMPIL_OPT = []
    for source in args2[1]:
        if os.path.splitext(source)[1] in SOURCE_EXTENSIONS:
            SOURCES.append(source)
        else: COMPIL_OPT.append(source)

    regions={}
    if args[0].hybrid:
      if not (args[0].regions_infos):
        fail_lec("--regions-infos needed with --hybrid.")
      if not (args[0].cere_objects):
        fail_lec("--cere-objects needed with --hybrid.")
      args[0].cere_objects = os.path.realpath(args[0].cere_objects)
      regions = read_file(args[0].hybrid_regions, args[0].regions_infos, SOURCES)

    if args[0].static:
        COMPIL_OPT.append("-static")

    for SOURCE in SOURCES:
        BASE, ext = os.path.splitext(SOURCE)
        OBJECT = args[0].o if args[0].o else BASE + '.o'

        # call mode_function
        first_compil(INCLUDES, SOURCE, BASE, ext, COMPIL_OPT)
        function[args[0].func](args[0], BASE, regions)
        last_compil(INCLUDES, SOURCE, BASE, OBJECT, COMPIL_OPT)

    global REGION_EXTRACTED, BACKEND_USE
    REGION_EXTRACTED = False
    BACKEND_USE="llc"
    #Compile extracted region with choosen flags
    objs = ""
    for region, data in regions.items():
      global BACKEND_FLAGS, MIDEND_FLAGS
      BACKEND_FLAGS = data['backend']
      MIDEND_FLAGS = data['midend']

      if args[0].extraction_lvl == "loop":
        last_compil(INCLUDES, "", region, region+'.o', COMPIL_OPT)
        objs = objs + ' ' + os.path.realpath(region+'.o')
      else:
        last_compil(INCLUDES, "", data["function"], data["function"]+'.o', COMPIL_OPT)
        objs = objs + ' ' + os.path.realpath(data["function"]+'.o')

    #Save new *.o files for link
    if objs:
      with open(args[0].cere_objects, "a") as text_file:
        text_file.write(objs)
