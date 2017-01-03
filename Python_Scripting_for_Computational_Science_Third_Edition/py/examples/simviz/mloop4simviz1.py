#!/usr/bin/env python
"""
Multiple loop script calling up simviz1.py. That is, the
user can assign multiple values to any of the legal input
parameters to simviz1.py and automatically get a loop over
all combinations of the input parameter values.

From each experiment we extract the maximum amplitude of
the oscillations and store this in a data structure
amplitude[p], where p is a tuple containing the values
of each input parameter.
"""

import sys, math, os
import scitools.multipleloop as mp
import scitools.filetable

# load command-line arguments into dictionary of legal prm names
p = {'m': 1, 'b': 0.7, 'c': 5, 'func': 'y', 'A': 5, 
     'w': 2*math.pi, 'y0': 0.2, 'tstop': 30, 'dt': 0.05}
# (case is not included since this parameter is overridden)
for i in range(len(sys.argv[1:])):
    name = sys.argv[i][1:]  # skip initial hyphen for prm name
    if name in p:
        p[name] = sys.argv[i+1]

#p = {'w': '[0.7:1.3,0.1]', 'b': '1 & 0.3 & 0', 'func': 'y & siny'}

prm_values = [(name, mp.input2values(p[name])) for name in p]
all, names, varied = mp.combine(prm_values)
for experiment in all:
    print experiment

options = mp.options(all, names, prefix='-')
for cmlargs in options:
    print cmlargs

def get_amplitude():
    # load data from sim.dat:
    t, y = scitools.filetable.readfile(os.path.join('tmp1','sim.dat'))
    amplitude = max(y[len(y)/2:])  # max of last half of y
    return amplitude

# add directory where simviz1.py resides to PATH:
os.environ['PATH'] += os.pathsep + \
   os.path.join(os.environ['scripting'], 'src','py','intro')

amplitude = []
# amplitude[i] equals (vprms, amp), where amp is the amplitude
# and vprms are the varied parameters, those with indicies
indices_varied = [names.index(i) for i in varied]

for cmlargs, parameters in zip(options, all):
    cmd = 'simviz1.py ' + cmlargs + ' -noscreenplot -case tmp1'
    failure = os.system(cmd)
    varied_parameters = [parameters[i] for i in indices_varied]
    amplitude.append((varied_parameters, get_amplitude()))

# plot amplitude as function of w:
i = names.index('w')
for p, a in amplitude:
    print p, a

    
