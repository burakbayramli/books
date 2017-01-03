#!/usr/bin/env python

"""
Integrates a system of ODEs in time.
The code follows the Fortran 77 program as closely as possible.
"""
import sys, math
import numpy as np

# parameters needed in this module are global variables:
m=1.0; b=1.0; c=1.0; A=1.0; w=1.0; y0=1.0; tstop=800.0; dt=0.05;
func = "y"
N = 2

def scan1():
    """
    Read input data (physical and numerical parameters)
    from standard input. Store data in global variables.
    """
    global m, b, c, A, w, y0, tstop, dt, func
    sys.stdin.readline() # read a blank line (in input file from simviz1.py)
    m = float(sys.stdin.readline())
    b = float(sys.stdin.readline())
    c = float(sys.stdin.readline())
    func = sys.stdin.readline().strip()  # strip off '\n'
    A = float(sys.stdin.readline())
    w = float(sys.stdin.readline())
    y0 = float(sys.stdin.readline())
    tstop = float(sys.stdin.readline())
    dt = float(sys.stdin.readline())

# no need for scan2 here - it was mainly made for exemplifying
# steering of oscillator.f from Python

def adv_fe(y, t, dt, scratch):
    """Advance the solution one step using the Forward Euler method"""
    n = len(y)
    scratch = rhs(y, t)
    for i in range(n):
        y[i] = y[i] + dt*scratch[i]
    return y  # not necessary, y is altered (call by ref.), but Python-ish

def adv_rk2(y, t, dt, scratch1, scratch2):
    """Advance the solution one step using the 2nd order R-K method"""
    n = len(y)
    scratch1 = rhs(y, t)
    for i in range(n):
        scratch2[i] = y[i] + dt*scratch1[i]
    t2 = t + dt
    scratch2 = rhs(scratch2, t2)
    for i in range(n):
        y[i] = y[i] + 0.5*dt*(scratch1[i] + scratch2[i])
    return y # not necessary, y is altered (call by ref.), but Python-ish

def rhs(y, t):
    """Define the right-hand side of the ODE system"""
    _y = y[0]
    if func == "y":
        cterm = _y
    elif func == "siny":
        cterm = math.sin(_y)
    elif func == "y3":
        cterm = _y - (_y**3)/6.0
    else:
        print "Error: spring term", func, "illegal"
        cterm = 0.0

    f = np.zeros(2)  # allocate array
    f[0] = y[1]
    f[1] = ( -b*y[1] - c*cterm + A*math.cos(w*t) )/m
    return f

def timeloop():
    """
    Integrate the ODEs in time by calling adv_fe or
    adv_rk2 at every time step
    """

    scratch1 = np.zeros(N);  scratch2 = np.zeros(N)
    y = np.zeros(N)

    # initial conditions:
    y[0] = y0;  y[1] = 0.0

    file = open('sim.dat', 'w')
    time = 0.0
    while time <= tstop:
        time += dt
        #y = adv_fe(y, time, dt, scratch1)
        y = adv_rk2(y, time, dt, scratch1, scratch2)
        file.write(" %7.4f%9.4f\n" % (time, y[0]))
    file.close()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1] == 'default':
            pass # no reading of input
        else:
            scan1()
    else:
        scan1()
    timeloop()
