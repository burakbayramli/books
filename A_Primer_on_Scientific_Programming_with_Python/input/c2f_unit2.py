#!/usr/bin/env python
"""
As temp-convert_v1.py, but we use functions for the
temperature conversion formulas.
"""

def C2F(C):
    F = (9.0/5)*C + 32
    return F

def F2C(F):
    return (F - 32)*(5/9.0)
    
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
    print '%.1fF' % C2F(C)
elif temp[-1] == 'F':
    # Temperature given in Fahrenheit
    F = float(temp[:-1])
    print '%.1fC' % F2C(F)
else:
    print 'Wrong unit (must have C or F, as in 21.2F or 20C)'
    sys.exit(1)





