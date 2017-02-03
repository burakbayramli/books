#!/usr/bin/env python
"""
Convert between Celsius and Fahrenheit degrees.
Example:
unix/DOS> python temp-convert.py 21C
69.8F
unix/DOS> python temp-convert.py 69.8F
21F
"""
import sys
try:
    temp = sys.argv[1]
except:
    print 'Usage: %s temperature' % sys.argv[0]
    print 'Example: %s 21C' % sys.argv[0]
    sys.exit(1)
if temp[-1] == 'C':
    # Temperature given in Celsius
    C = float(temp[:-1])
    F = (9.0/5)*C + 32
    print '%.1fF' % F
elif temp[-1] == 'F':
    # Temperature given in Fahrenheit
    F = float(temp[:-1])
    C = (F - 32)*(5/9.0)
    print '%.1fC' % C
else:
    print 'Wrong unit (must have C or F, as in 21.2F or 20C)'
    sys.exit(1)





