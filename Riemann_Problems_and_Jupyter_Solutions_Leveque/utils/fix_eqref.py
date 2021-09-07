
"""
Fix notebooks, replacing \eqref{label} with (\ref{label}).
"""

from __future__ import print_function
import os,sys,glob,re

regexp = re.compile(r"\eqref{(?P<eqlabel>[^}]*)}")

chapters = ['Preface',
            'Introduction',
            'Advection',
            'Acoustics',
            'Traffic_flow',
            'Burgers',
            'Nonconvex_scalar',
            'Shallow_water',
            'Shallow_tracer',
            'Euler',
            'Approximate_solvers',
             'Burgers_approximate',
             'Shallow_water_approximate',
             'Euler_approximate',
             'Euler_compare']

for notebook in chapters:
    file = notebook + '.ipynb'
    #newfile = 'NEW'+file  # for testing
    newfile = file  # overwrites the original file!
    infile = open(file,'r')
    lines = infile.read()
    infile.close()

    if lines.find('eqref') < 0:
        print("No change to ",file)
    else:
        print('Fixing ', file)
        infile = open(file,'r')
        lines = infile.readlines()
        infile.close()
        with open(newfile,'w') as outfile:
            for line in lines:
                if 'eqref' in line:
                    print('Line: ',line)

                    result = regexp.search(line)
                    while result:
                        label = result.group('eqlabel')
                        print('Fixing %s' % label)
                        line = line.replace(r'\\eqref{%s}' % label, r'(\\ref{%s})' % label)
                        result = regexp.search(line)
                    print('FIXED: ',line)
                outfile.write(line)
        outfile.close()

