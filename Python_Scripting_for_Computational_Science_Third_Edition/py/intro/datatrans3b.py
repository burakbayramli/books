#!/usr/bin/env python
import sys, math
from numpy import *
"""
As datatrans2.py, but using NumPy arrays and the special
TableIO module to increase the computational and I/O
efficiency.
"""

try:
    import TableIO.TableIO as TableIO
except:
    print """
The TableIO Python module was not found. Make sure it is
accessible in one of the directories in PYTHONPATH."""
    sys.exit(1)
    
try:
    infilename  = sys.argv[1]
    outfilename = sys.argv[2]
except:
    print "Usage:",sys.argv[0], "infile outfile"; sys.exit(1)

# read column 0 into a NumPy array x, column 1 into y,
# '#' is a valid comment sign in the input file:

x, y = TableIO.readColumns(infilename, '#', [0, 1])

# transform y values:

def myfunc(y):
    # y is a NumPy array
    return where(y < 0, 0.0, y**5*exp(-y))

y = myfunc(y)
# transform x and y to a single NumPy array and dump:
NumPy_xy = transpose(array([x,y]))
TableIO.writeArray(outfilename, NumPy_xy)
#TableIO.writeArray(outfilename, array([x,y]))  # rows inst. of columns

