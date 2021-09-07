
"""
Fix notebooks, replacing [X.ipynb](X.ipynb) with [X](X.ipynb)
for consistency everywhere.

Could be adapted to change in the other direction instead.
"""

from __future__ import print_function
import os,sys,glob,re

regexp = re.compile(r"\[(?P<nbtext>[^\]]*).ipynb\]\((?P<nb>[^\)]*)\)")

part0 = ['Preface']

part1 = ['Introduction',
            'Advection',
            'Acoustics',
            'Burgers',
            'Traffic_flow',
            'Nonconvex_scalar',
            'Shallow_water',
            'Shallow_tracer',
            'Euler']

part2 = ['Approximate_solvers',
         'Burgers_approximate',
         'Shallow_water_approximate',
         'Euler_approximate',
         'FV_compare']

chapters = part0 + part1 + part2
files = [f+'.ipynb' for f in chapters]

#files = ['Nonconvex_scalar.ipynb']

for f in files:
    #newfile = 'NEW'+f  # for testing
    newfile = f  # overwrites the original file!

    print('FILE: ',f)
    lines = open(f).read()
        
    if lines.find('.ipynb') < 0:
        print("No change to ",f)
    else:
        infile = open(f,'r')
        lines = infile.readlines()
        infile.close()
        with open(newfile,'w') as outfile:
            for line in lines:
                if '.ipynb' in line:
                    #print('Line: ',line)

                    result = regexp.search(line)
                    while result:
                        nb = result.group('nb')
                        nbtext = result.group('nbtext')
                        old_text = r'[%s.ipynb](%s)' % (nbtext, nb)
                        print('Fixing old text: %s' % old_text)
                        #nb_name = os.path.split(nb)[0]
                        new_text = r'[%s](%s)' % (nbtext,nb)
                        print('       new text: %s' % new_text)
                        line = line.replace(old_text, new_text)
                        result = regexp.search(line)
                    #print('FIXED: ',line)
                outfile.write(line)
        outfile.close()
