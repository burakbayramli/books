#!/usr/bin/env python

"""Reads and interprets (x,y,z) coordinates, reflecting
   a function z=f(x,y), defined on a regular xy-grid.
   The file format reads
 -1.0000e+00   -5.0000e-01     -1.500000e+00
 -5.0000e-01   -5.0000e-01     -1.000000e+00
  0.0000e+00   -5.0000e-01     -5.000000e-01
 -1.0000e+00   -2.5000e-01     -1.250000e+00
 -5.0000e-01   -2.5000e-01     -7.500000e-01
  0.0000e+00   -2.5000e-01     -2.500000e-01
 -1.0000e+00    0.0000e+00     -1.000000e+00
 -5.0000e-01    0.0000e+00     -5.000000e-01
  0.0000e+00    0.0000e+00      0.000000e+00
 -1.0000e+00    2.5000e-01     -7.500000e-01
 -5.0000e-01    2.5000e-01     -2.500000e-01
  0.0000e+00    2.5000e-01      2.500000e-01
 -1.0000e+00    5.0000e-01     -5.000000e-01
 -5.0000e-01    5.0000e-01      0.000000e+00

The result is a list of the z values, and Python variables
summarizing the grid (xmin, xmax, ymin, ymax, x-division,
y-division).
"""

import re, string, struct, sys

def generateData(datafile, nx, ny, xmin, xmax, ymin, ymax):
    dx = (xmax-xmin)/(nx-1)
    dy = (ymax-ymin)/(ny-1)
    for j in range(ny):
        for i in range(nx):
            x = xmin + i*dx
            y = ymin + j*dy
            z = x + y
            datafile.write("%(x)12.4e  %(y)12.4e  %(z)16.6e\n" % vars())

def filter(infile):
    last_x = 1.0E+20   # "infinity"
    nx = 0;            # no of points in x dir
    ny = 0;            # no of points in y dir
    data = []          # list of (x,y,z) coordinates
    while 1:
        line = infile.readline()
        if not line: break
        x, y, z = map(float, line.split())
        #or: x,y,z = map(float, string.strip(line))
        #x, y, z = map(float, re.split(r"\s+", line))
        data.append((x,y,z))

        if x < last_x:   # new y row
            if nx == 0:
                ymin = y; xmin = x
            ny = ny + 1
            nx = 0
        last_x = x
        nx = nx + 1
    ymax = y; xmax = x
    return [(xmin, xmax, ymin, ymax),(nx, ny), data]
    

try:
    nx = sys.argv[1]
    ny = sys.argv[2]
except:
    nx = 3; ny = 5
xmin = -1.0
ymin = -0.25
xmax = 1.0
ymax = 1.0
# first generate a file stuff.tmp with suitable data for conversion:
filename = "stuff.tmp"
datafile = open(filename, 'w')
generateData(datafile, nx, ny, xmin, xmax, ymin, ymax)
datafile.close()

# convert stuff.tmp, write result to stdout:
datafile = open(filename, 'r')
list = filter(datafile)
(xmin, xmax, ymin, ymax) = list[0]
(nx, ny) = list[1]
imax = nx-1; jmax = ny-1
gridline = "%% grid: [%(xmin)g,%(xmax)g]x[%(ymin)g,%(ymax)g] "\
           "for (i,j)=[0:%(imax)d]x[0:%(jmax)d]" % vars()

# output can be in ASCII of binary form, depending on the
# first command-line argument
binary = 0
try:
    if sys.argv[1] == 'binary':
        binary = 1
        print "binary output to file stuffb.tmp"
except: pass

if not binary:
    print gridline
    for (x,y,z) in list[2]:
        print z
else:
    # binary output
    datafile = open('stuffb.tmp', 'wb')
    datafile.write(gridline + '\n')
    # first the number of z values:
    datafile.write(struct.pack('i',nx*ny)) # C integer format
    # dump the z values:
    for (x,y,z) in list[2]:
        datafile.write(struct.pack('d',z)) # C double format
