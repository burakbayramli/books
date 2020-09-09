import matplotlib
matplotlib.use('TkAgg')
from pylab import *

import random as rd
n = 1000 # number of particles
sd = 0.1 # standard deviation of Gaussian noise

def initialize():
    global xlist, ylist
    xlist = []
    ylist = []
    for i in xrange(n):
        xlist.append(rd.gauss(0, 1))
        ylist.append(rd.gauss(0, 1))
    
def observe():
    global xlist, ylist
    cla()
    plot(xlist, ylist, '.')

def update():
    global xlist, ylist
    for i in xrange(n):
        xlist[i] += rd.gauss(0, sd)
        ylist[i] += rd.gauss(0, sd)

def num_particles (val = n):
    '''
    Number of particles.
    Make sure you change this parameter while the simulation is not running,
    and reset the simulation before running it. Otherwise it causes an error!
    '''
    global n
    n = int(val)
    return val

import pycxsimulator
pycxsimulator.GUI(parameterSetters = [num_particles]).start(func=[initialize, observe, update])
