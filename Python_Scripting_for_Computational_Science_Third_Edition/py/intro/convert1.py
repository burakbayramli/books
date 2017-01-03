#!/usr/bin/env python
import sys, math, string
usage = 'Usage: %s infile' % sys.argv[0]

try:
    infilename = sys.argv[1]
except:
    print usage; sys.exit(1)
    
ifile = open(infilename, 'r') # open file for reading

# read first comment line (no further use of it here):
line = ifile.readline()

# next line contains the increment in t values:
dt = float(ifile.readline())

# next line contains the name of the curves:
ynames = ifile.readline().split()

# list of output files:
outfiles = []
for name in ynames:
    outfiles.append(open(name + '.dat', 'w'))

t = 0.0    # t value
# read the rest of the file line by line:
for line in ifile:
    yvalues = line.split()
    if len(yvalues) == 0: continue  # skip blank lines
    for i in range(len(outfiles)):
        outfiles[i].write('%12g %12.5e\n' % \
                          (t, float(yvalues[i])))
    t += dt
for file in outfiles:  file.close()
