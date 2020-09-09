import matplotlib
matplotlib.use('TkAgg')
from pylab import *

n = 100     # size of grid: n * n
Dh = 1. / n # spatial resolution, assuming space is [0,1] * [0,1]
Dt = 0.01   # temporal resolution

mu_a = 0.001 # mobility of species a
mu_b = 0.001 # mobility of species b

xvalues, yvalues = meshgrid(arange(0, 1, Dh), arange(0, 1, Dh))

def initialize():
    global a, b, nexta, nextb
    # initial configuration
    a = exp(-((xvalues - 0.45)**2 + (yvalues - 0.45)**2) / (0.3**2))
    b = exp(-((xvalues - 0.55)**2 + (yvalues - 0.55)**2) / (0.1**2))
    nexta = zeros([n, n])
    nextb = zeros([n, n])
    
def observe():
    global a, b, nexta, nextb
    subplot(1, 2, 1)
    cla()
    imshow(a, vmin = 0, vmax = 1)
    title('a')
    subplot(1, 2, 2)
    cla()
    imshow(b, vmin = 0, vmax = 1)
    title('b')
    
def update():
    global a, b, nexta, nextb
    for x in xrange(n):
        for y in xrange(n):
            # state-transition function
            aC, aR, aL, aU, aD = a[x,y], a[(x+1)%n,y], a[(x-1)%n,y], \
                                 a[x,(y+1)%n], a[x,(y-1)%n]
            bC, bR, bL, bU, bD = b[x,y], b[(x+1)%n,y], b[(x-1)%n,y], \
                                 b[x,(y+1)%n], b[x,(y-1)%n]
            bLapNum = bR + bL + bU + bD - 4 * bC
            nexta[x,y] = aC + mu_a * ((aR-aL)*(bR-bL)+(aU-aD)*(bU-bD)
                                      +4*aC*bLapNum) * Dt/(4*Dh**2)
            nextb[x,y] = bC + mu_b * bLapNum * Dt/(Dh**2)

    a, nexta = nexta, a
    b, nextb = nextb, b

import pycxsimulator
pycxsimulator.GUI(stepSize = 50).start(func=[initialize, observe, update])
