#!/usr/bin/env python
"""
Take backup of files with specified extensions in specified
directory trees.

This is more or less a solution of exercise 3.17 (1st edition) or
3.16 (2nd edition) in H. P. Langtangen: "Python Scripting for
Computational Science". See that exercise for description of
functionality.
"""

from scitools.misc import find
import sys, re, os, shutil, string, fnmatch

if len(sys.argv) <= 1:
    print "Usage: backup.py tree1 tree2 .tex .sh tree3"
    print "(takes backup of all files with suffix .tex and .sh"
    print "in the tree1, tree2 and tree3 directory trees"
    sys.exit(1)
    
suffices = []
roots = []
for i in sys.argv[1:]:
    if i[0] == '.':
        suffices.append(i[1:])
    else:
        roots.append(i)

from time import strftime, localtime, time
def add_date (arg):
   return arg + strftime("_%b%d_%Y",localtime(time()))

copydir = add_date(os.path.join(os.environ['HOME'],'backup', 'copy'))
if not os.path.isdir(copydir):
    os.makedirs(copydir)
        
def copy(filename, suffices=['*'], age_limit=3):  # count age_limit in days
    # suffix is a Unix shell-style wildcard expression
    # (without the leading .)
    treat_file = 0
    filesuffix = string.split(filename, '.')[-1]
    for s in suffices:
        if fnmatch.fnmatch(filesuffix, s):
            treat_file = 1

    last_modification = os.path.getmtime(filename)
    age = time() - last_modification
    age = age/(60*60*24)  # convert seconds to days
    
    if treat_file and age <= age_limit:
        shutil.copy(filename, copydir)

        last_m = strftime("%a %b %d %H:%M:%S %Y",
                          localtime(last_modification))
        print "\ncopying %s\n  (last modified %s) to %s" % \
              (filename, last_m, copydir)

for dir in roots:
    find(copy, dir, suffices=suffices, age_limit=3)

# check if there are files in copydir
files = os.listdir(copydir)
nfiles = len(files)
if nfiles == 0:
    print "\n\nno files were copied"
    shutil.rmtree(copydir)
else:
    print "\n\n", len(files), "were copied"

