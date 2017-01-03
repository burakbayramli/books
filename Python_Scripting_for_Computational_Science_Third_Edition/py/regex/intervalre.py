#!/usr/bin/env python
"""
This file exemplifies regular expressions for intervals [r,s]
"""

import re

interval = r'\[\s*(-?\d+)\s*,\s*(-?\d+)\s*\]'
examples1 = ('[0,55]', '[ 0, 55  ]', '[-4, 55 ] ', '[r,s]')
for e in examples1:
    match = re.search(interval, e)
    if match:
        print e, 'matches!',
        lower_limit = int(match.group(1))
        upper_limit = int(match.group(2))
        print ' limits:', lower_limit, 'and', upper_limit
    else:
        print e, 'does not match'
print

# named groups:
interval = r'\[\s*(?P<lower>-?\d+)\s*,\s*(?P<upper>-?\d+)\s*\]'
examples1 = ('[0,55]', '[ 0, 55  ]', '[-4, 55 ] ', '[r,s]')
for e in examples1:
    match = re.search(interval, e)
    if match:
        print e, 'matches!',
        lower_limit = int(match.group('lower'))
        upper_limit = int(match.group('upper'))
        print ' limits:', lower_limit, 'and', upper_limit
    else:
        print e, 'does not match'
print

# real number in integer notation:
real_in = r'-?\d+'

# real number in scientific notation:
real_sn = r'-?\d(\.\d+|)[Ee][+\-]\d\d?'
#real_sn = r'-?\d\.?\d*[Ee][+\-]\d+'

# real number in decimal notation:
real_dn = r'-?(\d+\.\d*|\d*\.\d+)'
#real_dn = r'-?\d*\.\d*'

# regex for integer or real_sn or real_dn,
# with optional whitespace:
real = r'\s*(' + real_sn + '|' + real_dn + '|' + real_in + r')\s*'
# (always match the most complicated pattern first)

# regex for interval:
ir = r'\[' + real + ',' + real + r'\]'

# test:
examples2 = ('[2,9]', '[-44 , 1.54E-03]', '[3, 6.4]',
             '[-100,2.0e-1]', '[3.58652e+05 , 6E+09]',
             '[-.3, -0.9 ]', '[-87652, 1.9856e-004]')
print 'regex for interval:\n', ir
for e in examples2:
    match = re.search(ir,e)
    if match:
        print e, 'matches! ', 'groups=', match.groups()

# with less parenthesis (avoid OR constructions inside each
# form of notation):

# real number in scientific notation:
#real_sn = r'-?\d(\.\d+|)[Ee][+\-]\d\d?'
real_sn = r'-?\d\.?\d*[Ee][+\-]\d+'

# real number in decimal notation:
#real_dn = r'-?(\d+\.\d*|\d*\.\d+)'
real_dn = r'-?\d*\.\d*'

# regex for integer or real_sn or real_dn,
# with optional whitespace:
real2 = r'\s*(' + real_sn + '|' + real_dn + '|' + real_in + r')\s*'
# (always match the most complicated pattern first)

# regex for interval:
ir = r'\[' + real2 + ',' + real2 + r'\]'
print '\nregex for interval (avoid nested OR expressions):\n', ir
for e in examples2:
    match = re.search(ir,e)
    if match:
        print e, 'matches! ', 'groups=', match.groups()

# with named groups:
real1 = \
 r'\s*(?P<lower>' + real_sn + '|' + real_dn + '|' + real_in + r')\s*'
real2 = \
 r'\s*(?P<upper>' + real_sn + '|' + real_dn + '|' + real_in + r')\s*'
ir = r'\[' + real1 + ',' + real2 + r'\]'
print '\nregex for interval with named groups:\n', ir
for e in examples2:
    match = re.search(ir,e)
    if match:
        lower_limit = float(match.group('lower'))
        upper_limit = float(match.group('upper'))
        print e, 'matches! limits:', lower_limit, upper_limit
        print '   groups=', match.groups()

# regex with nested parenthesis (groups):
real_short = r'\s*(-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*'
ir_short = r'\[' + real_short + ',' + real_short + r'\]'

print '\nregex for interval:\n', ir_short
for e in examples2:
    match = re.search(ir_short,e)
    if match:
        print e, 'matches! ', 'groups=', match.groups()
print

# name the principal two groups:
real_short1 = \
 r'\s*(?P<lower>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*'
real_short2 = \
 r'\s*(?P<upper>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*'
ir_short = r'\[' + real_short1 + ',' + real_short2 + r'\]'

print '\nregex for interval using named 1st and 4th groups:\n', ir_short
for e in examples2:
    match = re.search(ir_short,e)
    if match:
        lower_limit = float(match.group('lower'))
        upper_limit = float(match.group('upper'))
        print e, 'matches! limits:', lower_limit, upper_limit
        print '   groups=', match.groups()
print


# we can get away with an extremely much simpler regex:
ir2 = r'\[(.*),(.*)\]'
for e in examples2:
    match = re.search(ir2,e)
    if match:
        print e, 'matches', ir2, 'group(1)=', match.group(1), \
              'group(2)=', match.group(2)
print


# try re.findall:

print 'with re.findall and real regex:', real
print '(nested OR expressions and many groups)'
for e in examples2:
    matches = re.findall(real, e)
    print 'findall applied to', e, 'gives', matches

print '\nwith re.findall and real regex:', real2
print '(no nested OR expressions and just one group)'
for e in examples2:
    matches = re.findall(real2, e)
    print 'findall applied to', e, 'gives', matches
print

real = \
 r'\s*(?P<number>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*'
print '\nwith re.findall and real regex:', real
print '(named outer (main) group, many groups)'
for e in examples2:
    c = re.compile(real)
    matches = c.findall(e)
    lower = matches[0][c.groupindex['number']]
    upper = matches[1][c.groupindex['number']]
    print 'findall applied to', e, 'gives', lower, 'and', upper
print


# named groups:
real = real_sn + '|' + real_dn + '|' + real_in
ir3 = r'\[\s*(?P<lower>' + real + ')\s*,\s*(?P<upper>' + real + r')\s*\]'
for e in examples2:
    match = re.search(ir3,e)
    if match:
        print e, 'matches!', 'group("lower")=', match.group("lower"), \
              "group('upper')=", match.group('upper')
        










