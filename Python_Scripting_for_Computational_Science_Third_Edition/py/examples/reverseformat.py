#!/usr/bin/env python
"""Reverse file conversion, from

% grid: [-1,1]x[-0.25,1] for (i,j)=[0:2]x[0:4]
-1.25 -0.25 0.75 -0.9375 0.0625
1.0625 -0.625 0.375 1.375 -0.3125 0.6875 1.6875
0.0 1.0 2.0

to

 -1.0000e+00   -2.5000e-01     -1.250000e+00
  0.0000e+00   -2.5000e-01     -2.500000e-01
  1.0000e+00   -2.5000e-01      7.500000e-01
 -1.0000e+00    6.2500e-02     -9.375000e-01
  0.0000e+00    6.2500e-02      6.250000e-02
  1.0000e+00    6.2500e-02      1.062500e+00
 -1.0000e+00    3.7500e-01     -6.250000e-01
  0.0000e+00    3.7500e-01      3.750000e-01
  1.0000e+00    3.7500e-01      1.375000e+00
 -1.0000e+00    6.8750e-01     -3.125000e-01
  0.0000e+00    6.8750e-01      6.875000e-01
  1.0000e+00    6.8750e-01      1.687500e+00
 -1.0000e+00    1.0000e+00      0.000000e+00
  0.0000e+00    1.0000e+00      1.000000e+00
  1.0000e+00    1.0000e+00      2.000000e+00

This script is the inverse of fileconversion.py
"""

import re, sys, string, struct
try:
    filename = sys.argv[1]
except IndexError:
    print 'Usage:', sys.argv[0], 'filename [binary]'; sys.exit(1)
binary = 0
try:
    if sys.argv[2] == 'binary':
        binary = 1
        print 'reading binary file', filename
except: pass

# distinguish between binary or text file (some OS might require this):
if binary:
    file = open(filename, 'rb')
else:
    file = open(filename, 'r')

try:
    firstline = file.readline()
except:
    print 'The file', filename, 'is empty!', sys.exit(1)

grid_regex = re.compile(r"""
    %\s*grid:\s*                 # opening part
    \[(.*),(.*)\]x\[(.*),(.*)\]  # use .* to match a number...
    .*                           # some text
    \[(.*):(.*)\]x\[(.*):(.*)\]
    """, re.VERBOSE)
m = grid_regex.search(firstline)
if m is None:
    print 'Wrong syntax on first line of', filename
    print firstline; sys.exit(1)
    
# m.group(1) contains the first backreference(as a string)
# m.group(2) the next one and so on
xmin = float(m.group(1));  xmax = float(m.group(2))
ymin = float(m.group(3));  ymax = float(m.group(4))
nx = int(m.group(6)) - int(m.group(5)) + 1
ny = int(m.group(8)) - int(m.group(7)) + 1
#print xmin, xmax, ymin, ymax, nx, ny

if not binary:
    lines = file.readlines()
    zvalues = []
    for line in lines:
        # can have more than one z value on each line
        zvalues += map(float, line.split())
else:
    data = file.read()  # read rest of file as a char chunk
    # first integer should be the number of z values:
    start = 0; stop = struct.calcsize('i')
    # struct.unpack always return tuples, even for a single number:
    nzvalues = (struct.unpack('i',data[start:stop]))[0]
    # load data into a typle of z values:
    format_nzvalues = str(nzvalues)+'d'
    start = stop; stop = start + struct.calcsize(format_nzvalues)
    zvalues = struct.unpack(format_nzvalues,data[start:stop])

# write out in three-column file format:
dx = (xmax-xmin)/(nx-1)
dy = (ymax-ymin)/(ny-1)
zcounter = 0
for j in range(ny):       # j=0,...,ny-1
    # x has fastest variation:
    for i in range(nx):   # i=0,...,nx-1
        x = xmin + i*dx
        y = ymin + j*dy
        print '%12.4e  %12.4e  %16.6e' % (x,y,zvalues[zcounter])
        zcounter = zcounter + 1




