#!/usr/bin/env python
import sys, math
from numpy import *
"""
As datatrans3a.py, but using Hinsen's Scientific.IO.ArrayIO module.
"""
    
try:
    infilename  = sys.argv[1]
    outfilename = sys.argv[2]
except:
    print "Usage:",sys.argv[0], "infile outfile"; sys.exit(1)

# read (x,y) data from file into a NumPy array data:
from Scientific.IO.ArrayIO import readArray, writeArray
data = readArray(infilename)

# transform y values:

def myfunc(y):
    # y is a NumPy array
    return where(y < 0, 0.0, y**5*exp(-y))

y = data[:,1]
y = myfunc(y)

# create a two-dimensional NumPy array with (x,myfunc(y)):
newdata = array([data[:,0], y]).transpose()

# dump the new data to file:
writeArray(newdata, outfilename)
# end
