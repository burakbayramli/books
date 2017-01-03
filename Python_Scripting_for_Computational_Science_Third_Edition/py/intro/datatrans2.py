#!/usr/bin/env python
import sys

try:
    infilename  = sys.argv[1]
    outfilename = sys.argv[2]
except:
    print "Usage:",sys.argv[0], "infile outfile"; sys.exit(1)
    
ifile = open(infilename, 'r')  # open file for reading
lines = ifile.readlines()      # read file into list of lines
ifile.close()

# go through each line and split line into x and y columns:
x = []; y = []   # store data pairs in two lists x and y
for line in lines:
    xval, yval = line.split()
    x.append(float(xval)); y.append(float(yval))

from math import exp
def myfunc(y):
    return (y**5*exp(-y) if y >= 0 else 0.0)

ofile = open(outfilename, 'w') # open file for writing
for i in range(len(x)):
    fy = myfunc(y[i])  # transform y value
    ofile.write('%g  %12.5e\n' % (x[i],fy))
ofile.close()
