#!/usr/bin/env python
"""interpret a comapct grid specification using regex"""
import re

# use a compact regular expression with nested OR expressions,
# and hence many groups, but name the outer (main) groups:
real_short1 = \
 r'\s*(?P<lower>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*'
real_short2 = \
 r'\s*(?P<upper>-?(\d+(\.\d*)?|\d*\.\d+)([eE][+\-]?\d+)?)\s*'
# regex for real interval [a,b] :
domain = r'\[' + real_short1 + ',' + real_short2 + r'\]'
# regex for integer interval [a:b] :
indices = r'\[\s*(-?\d+)\s*:\s*(-?\d+)\s*\]'

# test:
examples = ('domain=[0,10] indices=[0:11]',
            'domain=[0.1,1.1]x[0,2E+00] indices=[1:21]x[1:101]',
            '[0,1]x[0,2]x[-1,1.5] [1:21]x[1:11]x[-10:15]')
for ex in examples:
    print re.findall(indices, ex)
    # a nested list is returned; requires nested group counting
    print re.findall(domain, ex)
    print

# work with compiled expressions and the groupindex dictionary to
# extract the named groups easily from the nested list that is
# returned from re.findall:
print 'work with groupindex:'
for ex in examples:
    print re.findall(indices, ex)
    c = re.compile(domain)
    groups = c.findall(ex)
    intervals = []
    for i in range(len(groups)):
        intervals.append(
            (groups[i][c.groupindex['lower']-1],
             groups[i][c.groupindex['upper']-1]))
    print intervals
    print

# work with non-capturing parenthesis of the form (?:pattern)
real_short1 = \
 r'\s*(?P<lower>-?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:[eE][+\-]?\d+)?)\s*'
real_short2 = \
 r'\s*(?P<upper>-?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:[eE][+\-]?\d+)?)\s*'
# regex for real interval [a,b] :
domain = r'\[' + real_short1 + ',' + real_short2 + r'\]'
print 'non-capturing groups:'
for ex in examples:
    print re.findall(domain, ex)
    print

# avoid parenthesis, i.e., nested OR expressions:
real_sn = r'-?\d\.?\d*[Ee][+\-][0-9]+'
real_dn = r'-?\d*\.\d*'
real_in = r'-?\d+'
real1 = \
   r'\s*(?P<lower>' + real_sn + '|' + real_dn + '|' + real_in + ')\s*'
real2 = \
   r'\s*(?P<upper>' + real_sn + '|' + real_dn + '|' + real_in + ')\s*'
# regex for real interval [a,b] :
domain = r'\[' + real1 + ',' + real2 + r'\]'
# regex for integer interval [a:b] :
indices = r'\[\s*(-?\d+)\s*:\s*(-?\d+)\s*\]'
print '\navoid so many parenthesis (just two groups now for each interval):'
for ex in examples:
    print re.findall(indices, ex)
    print re.findall(domain, ex)
    print

# much simpler _working_ versions:
domain  = r'\[([^,]*),([^\]]*)\]'
indices = r'\[([^:,]*):([^\]]*)\]'
print '\nsimpler regular expressions:\n', domain, indices
for ex in examples:
    print re.findall(indices, ex)
    print re.findall(domain, ex)
    print

# these give wrong results
domain  = r'\[(.*?),(.*?)\]'
indices = r'\[(.*?):(.*?)\]'
print '\nalternative; simpler regular expressions:\n', domain, indices
for ex in examples:
    print re.findall(indices, ex)
    print re.findall(domain, ex)
    print
