import matplotlib
matplotlib.use('TkAgg')
from pylab import *

n = 100     # size of grid: n * n
Dh = 1. / n # spatial resolution, assuming space is [0,1] * [0,1]
Dt = 0.02   # temporal resolution

a, b, c, d, h, k = 1., -1., 2., -1.5, 1., 1. # parameter values

Du = 0.0001 # diffusion constant of u
Dv = 0.0006 # diffusion constant of v

def initialize():
    global u, v, nextu, nextv
    u = zeros([n, n])
    v = zeros([n, n])
    for x in xrange(n):
        for y in xrange(n):
            u[x, y] = 1. + uniform(-0.03, 0.03) # small noise is added
            v[x, y] = 1. + uniform(-0.03, 0.03) # small noise is added
    nextu = zeros([n, n])
    nextv = zeros([n, n])
    
def observe():
    global u, v, nextu, nextv
    subplot(1, 2, 1)
    cla()
    imshow(u, vmin = 0, vmax = 2, cmap = cm.binary)
    title('u')
    subplot(1, 2, 2)
    cla()
    imshow(v, vmin = 0, vmax = 2, cmap = cm.binary)
    title('v')
    
def update():
    global u, v, nextu, nextv
    for x in xrange(n):
        for y in xrange(n):
            # state-transition function
            uC, uR, uL, uU, uD = u[x,y], u[(x+1)%n,y], u[(x-1)%n,y], \
                                 u[x,(y+1)%n], u[x,(y-1)%n]
            vC, vR, vL, vU, vD = v[x,y], v[(x+1)%n,y], v[(x-1)%n,y], \
                                 v[x,(y+1)%n], v[x,(y-1)%n]
            uLap = (uR + uL + uU + uD - 4 * uC) / (Dh**2)
            vLap = (vR + vL + vU + vD - 4 * vC) / (Dh**2)
            nextu[x,y] = uC + (a*(uC-h) + b*(vC-k) + Du * uLap) * Dt
            nextv[x,y] = vC + (c*(uC-h) + d*(vC-k) + Dv * vLap) * Dt

    u, nextu = nextu, u
    v, nextv = nextv, v

import pycxsimulator
pycxsimulator.GUI(stepSize = 50).start(func=[initialize, observe, update])
