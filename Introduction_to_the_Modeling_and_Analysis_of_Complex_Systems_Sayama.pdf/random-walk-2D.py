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

import pycxsimulator
pycxsimulator.GUI().start(func=[initialize, observe, update])
