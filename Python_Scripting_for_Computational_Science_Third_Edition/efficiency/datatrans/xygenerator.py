#!/usr/bin/env python
# solution to exercise \ref{gui:pythontk:xygenerator}

"simple interface for creating input files for the datatrans*.py scripts"
import sys, re
# avoid requiring the user to write math.sin
# (just the plain sin(x) is more convenient):
from math import *
try:
    loop = sys.argv[1]
    func = sys.argv[2]
except:
    print "Usage: %s start:stop,step func" % sys.argv[0]
    print "Example: %s 0:1,0.0001 'x*sin(x)'" % sys.argv[0]
    sys.exit(1)

# this one extracts start:stop,step (we use .*, i.e., no test on numbers...)
match = re.search(r"(.*):(.*),(.*)", loop)
if match:
    start, stop, step = map(float, match.groups())

# or perhaps better: split wrt. : and ,
start, stop, step = [float(x) for x in re.split('[:,]', loop)]


# use x as independent variable (i.e., the function func must be
# expressed in terms of x)

x = start
while x <= stop:
    y = eval(func)   # the magic line...
    print x, y
    x += step

# note that as long as x has a value, eval(func) will evaluate
# the f(x) function given as command-line argument (provided
# f(x) is built of valid mathematical Python expressions)
