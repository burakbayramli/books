#!/usr/bin/env python
"""Numerical integration of circular motion."""
import math, sys, os

R=1; w=2*math.pi;  # global constants
def advance(x, y, dt, t):
    """advance (x,y) point one time step dt with Forward Euler,
       the equations describe circular motion of a body:
       dx/dt = -w*R*cos(2pi*w*t), dy/dt = w*R*sin(2pi*w*t)
    """
    x = x - dt*w*R*math.sin(w*t);  y = y + dt*w*R*math.cos(w*t)
    return (x,y)

# integrate from 0 to tstop
try:
    tstop = float(sys.argv[1]); dt = float(sys.argv[2])
except:
    print 'Usage: %s tstop dt' % sys.argv[0]; sys.exit(1)

# make output format compatible with the plotpairs.py script:
xmax = R*1.8; xmin = -xmax; ymin = xmin; ymax = xmax
print xmin, xmax, ymin, ymax
n = int(tstop/dt) + 1;
x = R; y = 0
for i in range(n):
    t = i*dt
    x, y = advance(x, y, dt, t)
    print x, y
print 'end'

    
