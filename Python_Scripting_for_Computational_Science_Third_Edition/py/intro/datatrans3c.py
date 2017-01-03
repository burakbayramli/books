#!/usr/bin/env python
import sys, math
from numpy import *
"""
As datatrans3a.py, but splitting the whole file at once,
store the numbers in a one-dimensional NumPy and then
reshaping the array appropriately.
"""
    
try:
    infilename  = sys.argv[1]
    outfilename = sys.argv[2]
except:
    print "Usage:",sys.argv[0], "infile outfile"; sys.exit(1)

# read (x,y) data from file into a NumPy array data:
f = open(infilename, 'r')
data = array(map(float, f.read().split()))
# (map is normally faster than [float(x) for x in f.read().split()])
data.shape = (len(data)/2,2)

# transform y values:

def myfunc(y):
    # y is a NumPy array
    return where(y < 0, 0.0, y**5*exp(-y))

x = data[:,0]
y = data[:,1]
y = myfunc(y)

f = open(outfilename, 'w')
import scitools.filetable
scitools.filetable.write_columns(f, x, y)
f.close()
# end
