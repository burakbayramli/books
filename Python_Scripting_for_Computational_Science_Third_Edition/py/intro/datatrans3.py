#!/usr/bin/env python
"""
As datatrans2.py, but using NumPy arrays and the special
filetable module to simplify the I/O. The difference between
datatrans3.py and datatrans3a/b/c/d.py is that datatrans3.py
employs a simpler function for transforming the y values
(the myfunc function in datatrans2.py needs special treatment
for vectorization).
"""
    
import sys
try:
    infilename  = sys.argv[1]
    outfilename = sys.argv[2]
except:
    print "Usage:",sys.argv[0], "infile outfile"; sys.exit(1)

# read (x,y) data from file into two NumPy arrays x and y:
import scitools.filetable
f = open(infilename, 'r')
x, y = scitools.filetable.read_columns(f)
f.close()

from scitools.all import *
x = 10*x
y = 2*y + 0.1*sin(y)

# dump the new data to file:
f = open(outfilename, 'w')
scitools.filetable.write_columns(f, x, y)
f.close()

# plot data:
plot(x, y)

# wait two seconds:
import time;  time.sleep(2)

# make two new curves and plot them:
x = linspace(0, 1, 1001)
y1 = sin(2*pi*x)
y2 = y1 + 0.2*sin(30*2*pi*x)
plot(x, y1, 'b-', x, y2, 'r-', legend=('sine', 'sine w/noise'),
     title='plotting arrays', xlabel='x', ylabel='y')

hardcopy('tmp1.ps')  # dump plot to file

# end
