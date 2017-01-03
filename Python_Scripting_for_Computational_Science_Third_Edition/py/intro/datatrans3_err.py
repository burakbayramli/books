#!/usr/bin/env python
"""
As datatrans3.py, but a potential error is introduced to
demonstrate debugging.
"""
    
import sys
infilename = sys.argv[1];  outfilename = sys.argv[2]
# read (x,y) data from file into two NumPy arrays x and y:
import scitools.filetable
f = open(infilename, 'r')
x, y = scitools.filetable.read_columns(f)
f.close()

# test exceptions (for debugging):
def f(x):
    p = x+1
    p[10] = 0
    return p
x = f(x)  # leads to an exception if len(x) < 11

x = 10*x
y = 2*y + 0.1*sin(y)

# dump the new data to file:
f = open(outfilename, 'w')
scitools.filetable.write_columns(f, x, y)
f.close()

