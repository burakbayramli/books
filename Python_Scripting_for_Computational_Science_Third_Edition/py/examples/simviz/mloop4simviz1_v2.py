#!/usr/bin/env python
"""
As mloop4simviz1.py, but we use the more high-level
class MultipleLoops rather than the basic functions
in the multipleloop module.
"""

import sys, math, os
import scitools.multipleloop
import scitools.filetable

# load command-line arguments into dictionary of legal prm names
p = {'m': 1, 'b': 0.7, 'c': 5, 'func': 'y', 'A': 5, 
     'w': 2*math.pi, 'y0': 0.2, 'tstop': 30, 'dt': 0.05}
# (case is not included since this parameter is overridden)
for i in range(len(sys.argv[1:])):
    name = sys.argv[i][1:]  # skip initial hyphen for prm name
    if name in p:
        p[name] = sys.argv[i+1]

experiments = scitools.multipleloop.MultipleLoop(option_prefix='-')
html = scitools.multipleloop.ReportHTML('tmp.html')
for name in p:
    experiments.add(name, p[name])

def get_amplitude(c):
    # load data from sim.dat:
    t, y = scitools.filetable.readfile(os.path.join('tmp%d' % c,'sim.dat'))
    amplitude = max(y[len(y)/2:])  # max of last half of y
    return amplitude

# add directory where simviz1.py resides to PATH:
os.environ['PATH'] += os.pathsep + \
   os.path.join(os.environ['scripting'], 'src','py','intro')

amplitude = []
# amplitude[i] equals (vprms, amp), where amp is the amplitude
# and vprms are the varied parameters

c = 1 # counter
for cmlargs, parameters, varied_parameters in experiments:
    cmd = 'simviz1.py ' + cmlargs + ' -noscreenplot -case tmp%d' % c
    failure = os.system(cmd)
    amplitude.append((varied_parameters, get_amplitude(c)))
    # report:
    html.experiment_section(parameters,
                            experiments.names,
                            experiments.varied)
    html.dump("""\n<IMG SRC=%s>""" % \
              os.path.join('tmp%d' % c, 'tmp%d.png' % c))
    c += 1

# plot amplitude as function of w:
for p, a in amplitude:
    print p, a

    
