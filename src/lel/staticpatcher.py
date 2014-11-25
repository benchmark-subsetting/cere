#!/usr/bin/env python
import subprocess
import sys
import os

def fatal(msg):
  print >>sys.stderr, msg
  sys.exit(1)

def sysout(cmd):
    return (subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
           .communicate()[0])

original=sys.argv[1]
topatch=sys.argv[2]
loop=sys.argv[3]

def parse_dumps(loop):
    secs = []
    for f in os.listdir("cere_dumps/{0}/".format(loop)):
        if f.endswith(".memdump"):
            secs.append(int(f.split(".")[0], 16))
    return sorted(secs)

def find_dump(addr, dumps):
    for i, a in enumerate(dumps):
        if addr < a: break
    candidate = dumps[i-1]
    if candidate < a: 
        return candidate
    else:
        return None


def find_section_offset(addr, elfmap):
    for s in elfmap:
        start = elfmap[s]["addr"] 
        end = elfmap[s]["size"] + start
        if addr >= start and addr < end:
            return elfmap[s]["offset"]+(addr-elfmap[s]["addr"])

def parse_sym(binary, elfmap):
    sym_map = {}
    elf = sysout("readelf -sW {0}".format(binary))
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
    elf = sysout("readelf -SW {0}".format(binary))
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

dumps = parse_dumps(loop)

os.system("objcopy --rename-section .bss=.bss,alloc,load {0} {0}".format(topatch))

original_map = parse_sym(original, parse_elf(original))
topatch_map = parse_sym(topatch, parse_elf(topatch))

with open(topatch, 'r+b') as topatch_file: 
    for s in original_map:
        if s not in topatch_map: continue
        assert(topatch_map[s]['size'] == original_map[s]['size']) 

        dump = find_dump(original_map[s]['addr'], dumps)
        if not dump:
            continue
        with open("cere_dumps/{0}/{1:012x}.memdump".format(loop, dump), "rb") as dump_file:
            # Seek in the original file
            try:
                dump_file.seek(original_map[s]['addr']-dump, 0)
            except:
                continue
            # Seek in the patched file
            topatch_file.seek(topatch_map[s]['offset'], 0)

            buf = dump_file.read(original_map[s]['size'])
            topatch_file.write(buf)
