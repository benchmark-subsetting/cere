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

import argparse
import os
import sys
from cere.vars import *
import cere.lec as lec
import cere.lel as lel

# By default, compile for C
COMPILER = CLANG

option_forbidden = set([("dump", "--instrument"),
                       ("dump", "--nested-regions"),
                       ("dump", "--no-nested-regions"),
                       ("dump", "--instrument-app"),
                       ("dump", "--hybrid"),
                       ("dump", "--static"),
                       ("replay", "--regions-file"),
                       ("replay", "--nested-regions"),
                       ("replay", "--no-nested-regions"),
                       ("replay", "--instrument-app"),
                       ("replay", "--hybrid")])

#to ignore prefix matching
class MyParser(argparse.ArgumentParser):
    def _get_option_tuples(self, option_string):
        return []

#Allowed option verification
class OptAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        if (parser.description, option_string) in option_forbidden:
            exit("Error:can't {option} in {mode} mode".format(
                 option=option_string, mode=parser.description))
        else:
            if (self.const):
                setattr(namespace, self.dest, self.const)
            else:
                setattr(namespace, self.dest, values)

#Initialize Includes parser
class IncludeAction(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        namespace.Inc = (namespace.Inc + [option_string + values])

def init_parser_incl(parser, liste):
    parser.set_defaults(Inc=[])
    for j in liste:
        parser.add_argument(j, action=IncludeAction)

# initialize subparsers
def init_subpars(parser, region_required):
    parser.add_argument('--region', action=OptAction, required=region_required)
    parser.add_argument('--regions-file', action=OptAction)
    parser.add_argument('--invocation', action=OptAction)
    parser.add_argument('--instrument', action=OptAction, nargs=0,
                        const=True, default=False)
    parser.add_argument('--instrument-app', action=OptAction, nargs=0,
                        const=True, default=False)
    parser.add_argument('--regions-infos', action=OptAction, default="")
    parser.add_argument('--hybrid-regions', action=OptAction, default="")
    parser.add_argument('--extraction-lvl', action=OptAction, default="")
    parser.add_argument('--cere-objects', action=OptAction, default="")
    parser.add_argument('--hybrid', action=OptAction, nargs=0,
                        const=True, default=False)
    parser.add_argument('--static', action=OptAction, nargs=0,
                        const=True, default=False)
    parser.add_argument('--wrapper', action=OptAction, default="")
    parser.add_argument('-o')
    parser.add_argument('-c', action=OptAction, nargs=0,
                        const=True, default=False)

def init_parser_core(parser):
    '''
    initialize main parser
    Create Subparsers for dump replay and original mode and
    manage version
    '''
    parser.add_argument('--version', '-v', action='version',
                        version='%(prog)s '+VERSION)
    subparsers = parser.add_subparsers(help='sub-command help')
    parser_dump = subparsers.add_parser('dump', description='dump')
    init_subpars(parser_dump, True)
    parser_dump.set_defaults(func="dump_fun")
    parser_replay = subparsers.add_parser('replay', description='replay')
    init_subpars(parser_replay, True)
    parser_replay.set_defaults(func="replay_fun")
    parser_instrument = subparsers.add_parser('original',
                                              description='original')
    init_subpars(parser_instrument, False)
    parser_instrument.set_defaults(func="original_fun")


def fail_frontend(error_message):
    exit("Error {prog} : {cmd}".format(prog='compiler_frontend', cmd=error_message))

def safe_system(command):
    '''
    Try-catch system call
    Verify system call and exit with appropriate error message
    '''
    ret = os.system(command)
    if(ret):
        fail_frontend("safe_system -> " + command)


def run(lang, argv):
    '''
    Main function
    '''

    # Select compiler depending on lang
    if lang == "c":
        COMPILER = CLANG
    elif lang == "cpp":
        COMPILER = CLANGPP
    elif lang == "fortran":
        if FORTRAN_SUPPORT:
            compiler = "flang"
        else:
            fail_lec("Fortran support disabled in this build (reconfigure & reinstall using --with-flang).")

    # Get CERE_MODE from environment variable
    # If CERE_MODE is not defined, we "short-circuit" the normal build process by
    # performing a simple compiler call by passing the flags directly to the LLVM frontend.
    # This avoids unnecessary compiltion steps that dump the .ll files and can cause issues,
    # for instance where build systems such as CMake and autotools build a bunch of test
    # programs to validate the compiler.
    try:
      cere_args = os.environ["CERE_MODE"].split()
    except KeyError:
      print("Compiling in normal mode (CERE_MODE undefined)")
      argv = " ".join(argv[1:])
      safe_system("{0} {1}".format(COMPILER, argv))
      exit(0)


    # Create parsers
    parser = MyParser()
    # This second parser does not remove prefix matching
    incl_parser = argparse.ArgumentParser()
    init_parser_core(parser)
    init_parser_incl(incl_parser, ["-I", "-D"])

    #Parse args
    args = parser.parse_known_args(cere_args + sys.argv[1:])
    args2 = incl_parser.parse_known_args(args[1])
    if args[0].c:
        lec.compile(args, args2, COMPILER)
    else:
        lel.link(args, COMPILER)
