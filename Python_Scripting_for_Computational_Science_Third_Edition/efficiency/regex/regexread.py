#!/usr/bin/env python
import re, sys, math
from numpy import zeros
# find no of lines in the file (= no of items in the array)
f = open(sys.argv[1], 'r')
nlines = 0
for line in f:
    nlines += 1
f.close()
n = int(math.ceil(nlines**(1/3.0)))  # careful rounding!
print n
a = zeros((n,n,n))

# read file into a:
f = open(sys.argv[1], 'r')
for line in f:
    m = re.search(r'\[(\d+),(\d+),(\d+)\]=(.*)', line)
    if m:
        a[int(m.group(1)),int(m.group(2)),int(m.group(3))] = float(m.group(4))
f.close()
print "a[1,1,3]=",a[1,1,3]
