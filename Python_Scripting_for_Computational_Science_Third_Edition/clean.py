#!/usr/bin/env python
"""
Run all (specialized) clean.sh scripts in src/ tree.
These scripts usually remove .o, .a, .so files as well as
other files that can easily be regenerated.
"""
import os

# run all (specialized) clean.sh scripts in subdirectories:
def run_clean_scripts(arg, directory, files):
    if 'clean.sh' in files:
        print 'cleaning', directory
        origdir = os.getcwd()
        os.chdir(directory)
        os.system("cat clean.sh; ./clean.sh")
        os.chdir(origdir)
os.path.walk(os.curdir, run_clean_scripts, None)

# clean specific files:
patterns = ('tmp*', '*tmp', '*.so',)
import fnmatch
def clean_files(patterns, directory, files):
    for file in files:
        pathname = os.path.join(directory, file)
        for pattern in patterns:
            if fnmatch.fnmatch(file, pattern):
                if os.path.isfile(pathname):
                    os.remove(pathname)
                    print 'removing', pathname
os.path.walk(os.curdir, clean_files, patterns)
