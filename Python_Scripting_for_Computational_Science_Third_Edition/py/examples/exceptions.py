#!/usr/bin/env python
"""examples on catching exceptions"""
import sys

gridfile = 'tmp.grid'

# make trial files:
f = open(gridfile, 'w')
f.write("""
1.0 1.1 1.2
s 1.3 1.4
""")
f.close()
f = open(gridfile+'2', 'w')
f.write("""1.0 1.1 1.2 1.2 1.4 1.5""")
f.close()

# recover silently from any failure:
try:
    f = open(gridfile, 'r')
    xcoor = [float(x) for x in f.read().split()]
except:
    n = 10; xcoor = [i/float(n) for i in range(n+1)]
    print 'recovered from any error...'

# recover from two types of exceptions:
try:
    f = open(gridfile, 'r')
    xcoor = [float(x) for x in f.read().split()]
except (IOError, ValueError):
    n = 10; xcoor = [i/float(n) for i in range(n+1)]
    print 'recovered from IOError or ValueError'
    print sys.exc_type, sys.exc_value

# treat each exception separately:
try:
    f = open(gridfile+'2', 'r')
    xcoor = [float(x) for x in f.read().split()]
except IOError:
    print gridfile, 'does not exist, default data are used'
    n = 10; xcoor = [i/float(n) for i in range(n+1)]
except ValueError:
    print gridfile, 'does not contain numbers only'
    sys.exit(1)
else:
    # continue execution after successful try
    print 'xcoor was successfully read from file', xcoor

# extract exception message as part of the except statment:
try:
    f = open(gridfile, 'r')
    xcoor = [float(x) for x in f.read().split()]
except IOError, message:
    print gridfile, 'does not exist, default data are used'
    n = 10; xcoor = [i/float(n) for i in range(n+1)]
    print message
except ValueError, message:
    print gridfile, 'does not contain numbers only'
    print message


