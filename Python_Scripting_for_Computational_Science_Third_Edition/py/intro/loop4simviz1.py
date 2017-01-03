#!/usr/bin/env python
"""calls simviz1.py with different m values (in a loop)"""
import sys, os
usage = 'Usage: %s m_min m_max m_increment [ simviz1.py options ]' \
        % sys.argv[0]

try:
    m_min = float(sys.argv[1])
    m_max = float(sys.argv[2])
    dm    = float(sys.argv[3])
except IndexError:
    print usage;  sys.exit(1)

simviz1_options = ' '.join(sys.argv[4:])

html = open('tmp_mruns.html', 'w')
html.write('<HTML><BODY BGCOLOR="white">\n')

m = m_min
while m <= m_max:
    case = 'tmp_m_%g' % m
    cmd = 'python simviz1.py %s -m %g -case %s' % \
          (simviz1_options, m, case)
    print 'running', cmd
    failure = os.system(cmd)
    html.write('<H1>m=%g</H1> <IMG SRC="%s">\n' \
               % (m,os.path.join(case,case+'.png')))
    m += dm
html.write('</BODY></HTML>\n')
