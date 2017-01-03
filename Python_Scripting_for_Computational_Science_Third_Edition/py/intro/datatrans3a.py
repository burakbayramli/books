#!/usr/bin/env python
import sys, math
from numpy import *
"""
As datatrans2.py, but using NumPy arrays and the special
filetable module to simplify the I/O.
"""
    
try:
    infilename  = sys.argv[1]
    outfilename = sys.argv[2]
except:
    print "Usage:",sys.argv[0], "infile outfile"; sys.exit(1)

# read (x,y) data from file into a NumPy arrays:
import scitools.filetable
f = open(infilename, 'r')
x, y = scitools.filetable.read_columns(f)
f.close()

# transform y values:

def myfunc(y):
    # y is a NumPy array
    return where(y < 0, 0.0, y**5*exp(-y))

y = myfunc(y)   # transform the whole y array

# dump the new data to file:
f = open(outfilename, 'w')
scitools.filetable.write_columns(f, x, y)
f.close()
# end
