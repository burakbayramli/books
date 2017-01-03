#!/usr/bin/env python
"""Scientific Hello World script using a module hw."""
import sys
from hw import hw1, hw2

try:
    r1 = float(sys.argv[1]);  r2 = float(sys.argv[2])
except:
    print 'Usage:', sys.argv[0], 'r1 r2'; sys.exit(1)
print 'hw1, result:', hw1(r1, r2)
print 'hw2, result: ',
hw2(r1, r2)

# test hw3:
from hw import hw3
s = hw3(r1, r2)
print 'hw3, result:', s
