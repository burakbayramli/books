#!/usr/bin/env python
import sys, math
import oscillator

# default values of input parameters:
m = 1.0; b = 0.7; c = 5.0; func = 'y'; A = 5.0; w = 2*math.pi
y0 = 0.2; tstop = 30.0; dt = 0.05

from numpy import zeros, linspace
maxsteps = 10000
#maxsteps = 10
n = 2
y = zeros((n,maxsteps))
step = 0; time = 0.0

import matplotlib.pyplot as plt
g1 = plt.figure()  # create Figure for (y(t),dy/dt) plot
g2 = plt.figure()  # create Figure for y(t) plot
ax1 = g1.add_subplot(1, 1, 1)  # create Axes object
ax1.set_xlabel('y')
ax1.set_ylabel('dy/dt')
ax2 = g2.add_subplot(1, 1, 1)  # create Axes object
ax2.set_xlabel('t')
ax2.set_ylabel('y(t)')

def setprm():
    oscillator.scan2(m, b, c, A, w, y0, tstop, dt, func)

def rewind(nsteps=0):
    global step, time
    if nsteps == 0:         # start all over again?
        step = 0
        time  = 0.0
    else:                   # rewind nsteps
        step -= nsteps
        time -= nsteps*dt

def psplot():
    g1.savefig('tmp_phaseplot_%d.ps' % step)
    g2.savefig('tmp_y1_%d.ps' % step)

# this is possible, but not recommended:
#    oscillator.data.m = m
#    oscillator.data.b = b
#    oscillator.data.c = c
#    oscillator.data.A = A
#    oscillator.data.w = w
#    oscillator.data.y0 = y0
#    oscillator.data.tstop= tstop
#    oscillator.data.dt = dt
# error:
#    oscillator.data.func = func
    
def run(nsteps):
    global step, time, y
    if step+nsteps > maxsteps:
        print 'no more memory available in y'; return

    y, step, time = oscillator.timeloop2(y, step, time, nsteps)
    print 'run %d steps up to step %d; current time=%g' % (nsteps,step,time)

    t = linspace(0.0, time, step+1)
    y1 = y[0,0:step+1]
    y2 = y[1,0:step+1]
    if len(t) != len(y1):
        raise ValueError, 'length of y1 (%d) and t (%d) are different' % \
              (len(y1), len(t))
    ax1.plot(y1, y2)
    ax2.plot(t, y1)

setprm()

steering_help = \
"""# example on an interactive session:
    setprm()     # set default values of parameters
    run(60)      # solve for the first 60 time steps
    w = math.pi  # change the frequency of the applied load
    setprm()     # update parameters (here w)
    run(120)
    A = 10       # change the amplitude of the applied load
    setprm()
    run(100)
"""

def _test():
    setprm()     # set default values of parameters
    run(60)      # solve for the first 60 time steps
    raw_input('CR: ')
    w = math.pi  # change the frequency of the applied load
    setprm()     # update parameters (here w)
    run(120)
    raw_input('CR: ')
    A = 10       # change the amplitude of the applied load
    setprm()
    run(100)
    psplot()
    
if __name__ == '__main__':
    _test()
    plt.show()



