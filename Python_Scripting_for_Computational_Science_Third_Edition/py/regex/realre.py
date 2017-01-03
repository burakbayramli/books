#!/usr/bin/env python
"""
This file exemplifies regular expressions for real numbers
"""

import re

# real number in integer notation:
real_in = r'-?\d+'

# real number in scientific notation:
real_sn = r'-?[0-9](\.[0-9]+|)[Ee][+\-][0-9][0-9]?'
# or
real_sn = r'-?\d(\.\d+|)[Ee][+\-]\d\d?'
# or with embedded comments:
real_sn_x = r"""
-?              # optional minus
\d(\.\d+|)      # a number like 1 or 1.4098
[Ee][+\-]\d\d?  # exponent, E-03, e-3, E+12
"""

# real number in decimal notation:
real_dn = r'-?(\d+\.\d*|\d*\.\d+)'

# regex for integer or real_sn or real_dn,
# with optional whitespace:
real = r'\s*(' + real_sn + '|' + real_dn + '|' + real_in + r')\s*'
# (always match the most complicated pattern first)

# (note: this one tests for int first and will, when used with
# findall, interpret 1.34 as 1 and .34)
s = 'some text, a=2.54E-05, inside a string'  # test string
real_wrong = r'\s*('+real_in+'|'+real_dn+'|'+real_sn+r')\s*'
m = re.search(real_wrong, s)
print '\nerror: wrong number', m.group(0), \
      "is extracted from '%s'" % s, '\nall groups:', m.groups()

real_wrong = r'\s*('+real_dn+'|'+real_sn+'|'+real_in+r')\s*'
m = re.search(real_wrong, s)
print 'error: wrong number', m.group(0),\
      "is extracted from '%s'" % s, '\nall groups:', m.groups()

# shortened regex for real numbers:
real_short = r'-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?'
m = re.search(real_short, s)
print "\ncorrect number", m.group(0),\
      "is extracted from '%s'" % s, "\nall groups:", m.groups()











