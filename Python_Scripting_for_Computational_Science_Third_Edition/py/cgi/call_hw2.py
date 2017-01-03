#!/usr/bin/env python
"""Front-end script to hw2.py.cgi."""
import urllib, sys, re
r = float(sys.argv[1])
params = urllib.urlencode({'r': r})
URLroot = 'http://www.ifi.uio.no/~inf3330/scripting/src/py/cgi/'
f = urllib.urlopen(URLroot + 'hw2.py.cgi?' + params)
# grab s (=sin(r)) from the output HTML text:
for line in f.readlines():
    m = re.search(r'"equalsbutton">(.*)$', line)
    if m:
        s = float(m.group(1)); break
print 'Hello, World! sin(%g)=%g' % (r,s)

