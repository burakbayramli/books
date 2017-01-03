#!/usr/bin/env python
import os

"""A variant of the bash/csh which command. Here, which('matlab') returns
0 if matlab is not found on the system and otherwise 1.

    from somelib import which
    if which('matlab'):
        # matlab is present, do stuff with it
"""

def which(program):
    found = 0
    for dir in re.split(':', os.environ['PATH']):
        if os.path.isdir(dir): # skip non-existing directories
            if os.path.isfile(os.path.join(dir,program)):
                found = 1
                path = dir
                break
    return found

def test_which(p):
    print p, "is",
    if which(p): s=""
    else: s="not"
    print s," found"
    
if __name__ == '__main__':
    test_which('matlab')
    test_which('simres2gnuplot')
    for i in sys.argv[1:]:
        test_which(i)

    
