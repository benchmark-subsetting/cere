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
import re
import subprocess
import tempfile

from cere.vars import *
from cere.compress import compress

# By default, compile for C
COMPILER = CLANG

def grep(path, regex):
    regex=".*"+regex+".*"
    regObj = re.compile(regex)
    res = []
    for root, dirs, fnames in os.walk(path):
        for fname in fnames:
            if fname.endswith(".ll"):
                with open(fname, "r") as f:
                    for line in f.readlines():
                        if regObj.match(line):
                            res.append(os.path.join(root, fname))
    return res

def fail_lel(error_message):
    exit("Error {prog} : {cmd}".format(prog='lel', cmd=error_message))

def safe_system(command):
    '''
    Try-catch system call
    Verify system call and exit with appropriate error message
    '''
    ret = os.system(command)
    if(ret):
        fail_lel("safe_system -> " + command)

def user_main(FILE, LOOP, mode_opt):
    # Define by default TIMEOUT_SECONDS to 60
    TIMEOUT_SECONDS=os.getenv('TIMEOUT_SECONDS', 60) #1min is probably enough
    # Define by default CALL_COUNT to 10
    CERE_REPLAY_REPETITIONS=int(os.getenv('CERE_REPLAY_REPETITIONS', 10))
    TEMPLATE = open("{root}/template_realmain.c".format(root=ROOT), "r")
    template = TEMPLATE.read()
    TEMPLATE.close()
    FILE.write(template.format(time_out=TIMEOUT_SECONDS,
                               in_vitro_call_count=CERE_REPLAY_REPETITIONS,
                               loop=LOOP))

def baremetal_user_main(FILE, LOOP, mode_opt):
    # Define by default TIMEOUT_SECONDS to 60
    TIMEOUT_SECONDS=os.getenv('TIMEOUT_SECONDS', 60) #1min is probably enough
    # Define by default CALL_COUNT to 10
    CERE_REPLAY_REPETITIONS=int(os.getenv('CERE_REPLAY_REPETITIONS', 10))
    TEMPLATE = open("{root}/template_baremetal_realmain.c".format(root=ROOT), "r")
    template = TEMPLATE.read()
    TEMPLATE.close()
    FILE.write(template.format(time_out=TIMEOUT_SECONDS,
                               in_vitro_call_count=CERE_REPLAY_REPETITIONS,
                               loop=LOOP))

def create_user_main(mode_opt,LOOP):
    safe_system("rm -f realmain.c")
    MAIN=open('realmain.c','w')
    user_main(MAIN, LOOP, mode_opt)
    MAIN.close()

def create_baremetal_user_main(mode_opt,LOOP):
    safe_system("rm -f baremetal_realmain.c")
    MAIN=open('baremetal_realmain.c','w')
    baremetal_user_main(MAIN, LOOP, mode_opt)
    MAIN.close()


def compile_memory_dump_objects(mode_opt, DIR):
    if not mode_opt.static:
        DUMP_ARGS="-Wl,--section-start=.text=0x60004000 -Wl,--section-start=.init=0x60000000,--section-start=.dynsym=0x60002000"
    else:
        DUMP_ARGS="-Wl,--section-start=.text=0x6004000 -Wl,--section-start=.init=0x6000000,--section-start=.dynsym=0x6002000"
    safe_system("rm -f {0}/objs.S".format(DIR))
    OBJ=open("{0}/objs.S".format(DIR),'w')
    for FILE in os.listdir(DIR):
        BASE, ext = os.path.splitext(FILE)
        FILE = DIR+"/"+FILE
        if (ext=='.memdump'):
            OBJ.write((".section s{n}, \"aw\"\n" +
                       ".incbin \"{m}\"\n").format(n=BASE,m=FILE))
            DUMP_ARGS = (DUMP_ARGS +
                        " -Wl,--section-start=s{n}=0x{n}".format(n=BASE))
    OBJ.close()
    safe_system(CLANG + " -c {0}/objs.S".format(DIR))
    return DUMP_ARGS

def dump_fun(mode_opt, BINARY, COMPIL_OPT):
    '''
    Dump mode
    Call the linker and copy the original binary
    '''
    safe_system(("{link} {opts} -o {binary} {libdir} " +
                 "-Wl,-z,now -lcere_dump -ldl"
    ).format(link=COMPILER, binary=BINARY,
             opts=COMPIL_OPT, Root=PROJECT_ROOT,libdir=LIBDIR_FLAGS))

def sysout(cmd):
    return (subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
           .communicate()[0])

def find_section_offset(addr, elfmap):
    for s in elfmap:
        start = elfmap[s]["addr"]
        end = elfmap[s]["size"] + start
        if addr >= start and addr < end:
            return elfmap[s]["offset"]+(addr-elfmap[s]["addr"])

def parse_sym(binary, elfmap):
    sym_map = {}
    elf = sysout("readelf -sW {0}".format(binary)).decode('utf-8')
    elf_lines = elf.split('\n')

    for i, l in enumerate(elf_lines):
        if ".symtab" in l:
            start = i+2
            break

    for l in elf_lines[start:]:
        fields = l.split()
        if not fields: break
        if fields[3] != "OBJECT": continue
        if fields[5] != "DEFAULT": continue
        name = fields[-1]
        addr = int(fields[1],16)

        # Ignore GLIBC objects
        if "@@" in name:
            continue

        if fields[2].startswith('0x'):
            size = int(fields[2], 16)
        else:
            size = int(fields[2])

        off = find_section_offset(addr, elfmap)
        if not off:
            # ignore non static objs
            continue

        sym_map[name] = dict(addr=addr, offset=off, size=size)

    return sym_map

def parse_elf(binary):
    elf_map = {}
    elf = sysout("readelf -SW {0}".format(binary)).decode('utf-8')
    elf_lines = elf.split('\n')
    for l in elf_lines:
        l = l.strip()
        # Only parse section lines
        if not l.startswith('['): continue
        # Keep everything after ]
        cols = l.split(']')[1]
        fields = cols.split()
        name = fields[0]
        # Ignore non static sections
        if name not in [".data", ".fini_array", ".init_array", ".bss"]: continue
        addr = int(fields[2],16)
        offset = int(fields[3],16)
        size = int(fields[4],16)
        elf_map[name] = dict(addr=addr, offset=offset, size=size)
    return elf_map

def memdumps_to_objects(dir):
    # When compiling in baremetal replay mode,
    # we want to save memdumps into object files
    # in order to embed them into the executable

    owd = os.getcwd()
    os.chdir(dir)

    memchunks_sizes = b'' # Will contain usigned integer entries of 4 bytes (uint32_t)
    memchunks_addresses = b'' # Will contain addresses of each memchunk (read from filename)
    concatenated_memchunks = b'' # Will contain all memdumps one after another

    # Iterate over dump files.
    # We will concatenate the memdumps and save each of their sizes and addresses
    # This is needed because in the template, we must know the name of symbols
    # we access, so we can't just create a bunch of symbols for each dump file
    for f_name in os.listdir("."):
        if f_name.endswith(".memdump"):
            address = bytearray.fromhex(f_name[:-8])
            memchunk = open(f_name, "rb").read()
            memchunks_sizes += len(memchunk).to_bytes(length=4, byteorder='little') # assumes a little endian replay env
            memchunks_addresses += address
            concatenated_memchunks += memchunk

    # Put raw byte sequences in tmp files
    with open('memchunks_sizes', 'wb') as w:
        w.write(memchunks_sizes)
    with open('memchunks_addresses', 'wb') as w:
        w.write(memchunks_addresses)
    with open('concatenated_memchunks', 'wb') as w:
        w.write(concatenated_memchunks)

    # Generate chunk sizes object from tmp file
    safe_system(("{objcopy} --input binary"
    + " --output-target elf64-x86-64 --binary-architecture i386:x86-64"
    + " {filename} {filename}.o"
    + " --rename-section .data=.{filename},CONTENTS,ALLOC,LOAD,READONLY,DATA").format(
    objcopy=OBJCOPY, filename="memchunks_sizes"))

    # Generate chunk addresses object from tmp file
    safe_system(("{objcopy} --input binary"
    + " --output-target elf64-x86-64 --binary-architecture i386:x86-64"
    + " {filename} {filename}.o"
    + " --rename-section .data=.{filename},CONTENTS,ALLOC,LOAD,READONLY,DATA").format(
    objcopy=OBJCOPY, filename="memchunks_addresses"))

    # Generate concatenated chunks object from tmp file
    safe_system(("{objcopy} --input binary"
    + " --output-target elf64-x86-64 --binary-architecture i386:x86-64"
    + " {filename} {filename}.o"
    + " --rename-section .data=.{filename},CONTENTS,ALLOC,LOAD,READONLY,DATA").format(
    objcopy=OBJCOPY, filename="concatenated_memchunks"))

    # Remove tmp files
    safe_system("rm memchunks_sizes memchunks_addresses concatenated_memchunks")

    os.chdir(owd)


def maps_to_objects(dir):
    # As for the memdump files, the map files
    # will need to be embedded in ELF sections.
    # This will be simpler, as we only have 2
    # known files

    owd = os.getcwd()
    os.chdir(dir)

    if not os.path.isfile("core.map"):
        exit("Error: core.map not found")

    if not os.path.isfile("hotpages.map"):
        exit("Error: hotpages.map not found")

    # Generate core.o from core.map
    safe_system(("{objcopy} --input binary"
    + " --output-target elf64-x86-64 --binary-architecture i386:x86-64"
    + " {filename}.map {filename}.o"
    + " --rename-section .data=.{filename},contents,alloc,load,readonly,data").format(
    objcopy=OBJCOPY, filename="core"))

    # Generate hotpages.o from hotpages.map
    safe_system(("{objcopy} --input binary"
    + " --output-target elf64-x86-64 --binary-architecture i386:x86-64"
    + " {filename}.map {filename}.o"
    + " --rename-section .data=.{filename},contents,alloc,load,readonly,data").format(
    objcopy=OBJCOPY, filename="hotpages"))

    os.chdir(owd)


def extract_symbols(DIR):
    # get list of static names
    original = DIR + "/lel_bin"
    original_map = parse_sym(original, parse_elf(original))
    with open((DIR + "/static.names").encode(), "w") as f:
        for n in original_map:
            f.write(n + "\n")

    safe_system(("{objcopy} --globalize-symbols={dump_dir}/static.names"
                + " {dump_dir}/lel_bin {dump_dir}/static.sym").format(
                    objcopy=OBJCOPY, dump_dir=DIR))

    safe_system(("{objcopy} -S --extract-symbol"
                + " --keep-symbols={dump_dir}/static.names"
                + " {dump_dir}/static.sym {dump_dir}/static.sym").format(
                    objcopy=OBJCOPY, dump_dir=DIR))


#in replay mode
def replay_fun(mode_opt, BINARY, COMPIL_OPT):
    '''
    Replay mode
    Create Realmain and call the linker
    '''
    LOOP=mode_opt.region
    INVOCATION=mode_opt.invocation
    if(not INVOCATION):
        INVOCATION=1

    if mode_opt.static:
        COMPIL_OPT += " -static"

    if mode_opt.instrument and not mode_opt.wrapper:
        fail_lel("When using --instrument you must provide the --wrapper argument")
    #Find the .cere directory
    DIR="{dump_dir}/{loop}/{invocation}".format(dump_dir=CERE_DUMPS_PATH, loop=LOOP, invocation=INVOCATION)

    # Check that dumps exists
    if (not os.path.isdir(DIR)):
        fail_lel("No dump for {loop} invocation {invocation} in {dump_path}".format(loop=LOOP,
                 invocation=INVOCATION, dump_path=DIR))
    # Compress & extract the traces
    compress(DIR)
    create_user_main(mode_opt,LOOP)

    # Extract symbol file

    extract_symbols(DIR)

    for f in COMPIL_OPT.split():
        if f.endswith('.o'):
            safe_system (
                "{objcopy} --weaken-symbols={dump_dir}/static.names {f}".format(
                objcopy=OBJCOPY, dump_dir=DIR, f=f))



    safe_system(CLANG + " -c realmain.c")
    OPTS = compile_memory_dump_objects(mode_opt, DIR)
    with tempfile.NamedTemporaryFile() as f:
        f.write(("{opts} -o {binary}  -Wl,--just-symbols={dump_dir}/static.sym"
                 + " objs.o realmain.o {args} "
                 + " {libdir} -lcere_load -Wl,-z,now"
                 + " -ldl {wrapper}\n").format(
                opts=OPTS, binary=BINARY, args=COMPIL_OPT, Root=PROJECT_ROOT,
                     libdir=LIBDIR_FLAGS, wrapper=mode_opt.wrapper, dump_dir=DIR).encode())

        f.flush()
        safe_system(("{clang} @{tempfile}".format(tempfile=f.name, clang=CLANGPP)))

#in replay mode for baremetal
def baremetal_replay_fun(mode_opt, BINARY, COMPIL_OPT):
    '''
    Replay mode for baremetal
    Create Realmain and call the linker. We will use a specific Realmain template,
    and force static linking with a baremetal version fo the replay lib. At link time,
    we will also embed memory dump into the binary.
    '''

    print("Linking baremetal replay wrapper")
    LOOP=mode_opt.region
    INVOCATION=mode_opt.invocation
    if(not INVOCATION):
        INVOCATION=1

    # In baremetal mode, we will always link statically and disable
    COMPIL_OPT += " -static"

    if mode_opt.instrument and not mode_opt.wrapper:
        fail_lel("When using --instrument you must provide the --wrapper argument")
    #Find the .cere directory
    cere_dir = find_cere_dir()
    if not cere_dir:
      fail_lel("Failed to find .cere directory. try export CERE_WORKING_PATH=\"path/to/.cere/\"")
    DIR="{source_dir}/{dump_dir}/{loop}/{invocation}".format(source_dir=cere_dir, dump_dir=CERE_DUMPS_PATH, loop=LOOP, invocation=INVOCATION)

    # Check that dumps exists
    if (not os.path.isdir(DIR)):
        fail_lel("No dump for {loop} invocation {invocation} in {dump_path}".format(loop=LOOP,
                 invocation=INVOCATION, dump_path=DIR))

    # Compress & extract the traces
    compress(DIR)
    memdumps_to_objects(DIR)
    maps_to_objects(DIR)
    create_baremetal_user_main(mode_opt,LOOP)

    # Extract symbol file
    extract_symbols(DIR)

    for f in COMPIL_OPT.split():
        if f.endswith('.o'):
            safe_system (
                "{objcopy} --weaken-symbols={dump_dir}/static.names {f}".format(
                objcopy=OBJCOPY, dump_dir=DIR, f=f))


    safe_system(CLANG + " -nostdlib -c baremetal_realmain.c")
    OPTS = compile_memory_dump_objects(mode_opt, DIR)
    with tempfile.NamedTemporaryFile() as f:
        f.write(("{opts} -o {binary}  -Wl,--just-symbols={dump_dir}/static.sym"
                 + " {dump_dir}/memchunks_sizes.o {dump_dir}/memchunks_addresses.o {dump_dir}/concatenated_memchunks.o"
                 + " {dump_dir}/core.o {dump_dir}/hotpages.o"
                 + " objs.o baremetal_realmain.o {args} "
                 + " {libdir} -lcere_baremetal_load -Wl,-z,now"
                 + " -ldl {wrapper}\n").format(
                opts=OPTS, binary=BINARY, args=COMPIL_OPT, Root=PROJECT_ROOT,
                     libdir=LIBDIR_FLAGS, wrapper=mode_opt.wrapper, dump_dir=DIR).encode())

        f.flush()
        safe_system(("{clang} -nostdlib @{tempfile}".format(tempfile=f.name, clang=CLANG)))

#in original mode
def original_fun(mode_opt, BINARY, COMPIL_OPT):
    '''
    Original mode
    Only call the linker
    '''

    if mode_opt.static:
        COMPIL_OPT += "-static"

    if(mode_opt.instrument_app):
        safe_system(("{link} -o {binary} {opts} {libs} {libdir}").format(
              link=COMPILER, binary=BINARY, opts=COMPIL_OPT, libs=PROFILE_LIB,
              libdir=LIBDIR_FLAGS))
    elif(mode_opt.instrument):
        safe_system(("{link} -o {binary} {opts} {wrapper} {libdir}").format(
              link=COMPILER, binary=BINARY, opts=COMPIL_OPT,
              wrapper=mode_opt.wrapper, libdir=LIBDIR_FLAGS))
    else:
        safe_system(("{link} -o {binary} {opts}").format(link=COMPILER,
                binary=BINARY, opts=COMPIL_OPT))



def link(args, new_compiler):
    function={}
    function["replay_fun"] = replay_fun
    function["baremetal_replay_fun"] = baremetal_replay_fun
    function["dump_fun"] = dump_fun
    function["original_fun"] = original_fun

    COMPILER = new_compiler

    if (len(args[1]) == 0):
        exit("Error:Need source file")
    objs = ""
    if os.path.isfile(args[0].cere_objects):
      with open(args[0].cere_objects, "r") as text_file:
        objs = text_file.read()
    COMPIL_OPT = objs + ' ' + " ".join(args[1])

    if isinstance(args[0].o, type(None)):
        BINARY = "a.out"

    else:
        BINARY = args[0].o

    # call mode_function
    function[args[0].func](args[0], BINARY, COMPIL_OPT)
