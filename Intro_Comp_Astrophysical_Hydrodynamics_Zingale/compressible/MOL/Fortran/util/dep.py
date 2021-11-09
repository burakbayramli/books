#!/usr/bin/env python

# automatically generate Makefile dependencies for Fortran 90 source.
#
# this will output all the dependency pairs amongst the source files.
#
# M. Zingale (2012-03-21)

import re
import os
import argparse

# modules to ignore in the dependencies
IGNORES = ["iso_c_binding", "iso_fortran_env", "omp_lib", "mpi"]

# regular expression for "{}module{}name", where {} can be any number
# of spaces.  We use 4 groups here, denoted by (), so the name of the
# module is the 4th group
module_re = re.compile(r"^(\s*)([Mm][Oo][Dd][Uu][Ll][Ee])(\s+)((?:[a-z][a-z_0-9]+))",
                       re.IGNORECASE|re.DOTALL)

# regular expression for "{}module{}procedure{}name"
module_proc_re = re.compile(r"(\s*)(module)(\s+)(procedure)(\s+)((?:[a-z][a-z_0-9]+))",
                            re.IGNORECASE|re.DOTALL)

# regular expression for "{}use{}modulename...".  Note this will work for
# use modulename, only: stuff, other stuff'
# see (txt2re.com)
use_re = re.compile(r"^(\s*)([Uu][Ss][Ee])(\s+)((?:[a-z_][a-z_0-9]+))",
                    re.IGNORECASE|re.DOTALL)

def doit(prefix, search_path, files):

    # first parse the files and find all the module statements.  Keep a
    # dictionary of 'module name':filename.
    modulefiles = {}

    all_files = []

    for cf in files:
        
        # find the file in the first part of the search path it exists
        for p in search_path:
            full_file = "{}/{}".format(p, cf)
            if os.path.isfile(full_file): 
                all_files.append(full_file)
                break

        f = open(full_file, "r")
        
        for line in f:

            # strip off the comments
            idx = line.find("!")
            line = line[:idx]

            rebreak = module_re.search(line)
            rebreak2 = module_proc_re.search(line)
            if rebreak and not rebreak2:
                modulefiles[rebreak.group(4)] = cf

        f.close()
        
    # go back through the files now and look for the use statements.
    # Assume only one use statement per line.  Ignore any only clauses.
    # Build a list of dependencies for the current file and output it.
    for cf in all_files:

        f = open(cf, "r")

        for line in f:

            # strip off the comments
            idx = line.find("!")
            line = line[:idx]

            rebreak = use_re.search(line)
            if rebreak:
                print prefix+os.path.basename(cf).replace(".f90", ".o"), ':', \
                    prefix+os.path.basename(modulefiles[rebreak.group(4)]).replace(".f90", ".o")

        f.close()
        print " "

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("--prefix",
                        help="prefix to prepend to each dependency pair, e.g., for a build directory",
                        default="")
    parser.add_argument("--search_path",
                        help="ordered path to search for the files",
                        default="")
    parser.add_argument("files", metavar="source files", type=str, nargs="*",
                        help="F90 source files to find dependencies amongst")

    args = parser.parse_args()

    doit(args.prefix, args.search_path.split(), args.files)



