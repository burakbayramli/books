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
    savefig(str(t) + '.png')

def update():
    global xlist, ylist
    for i in xrange(n):
        xlist[i] += rd.gauss(0, sd)
        ylist[i] += rd.gauss(0, sd)

t = 0
initialize()
observe()
for t in xrange(1, 100):
    update()
    observe()
