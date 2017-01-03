#!/usr/bin/env python
"""
Functions for solving a spherically symmetric 1D wave equation.
As wave1D_func1.py, but v=u*r is the unknown in the PDE.
"""
from __future__ import division  # disable integer division
from scitools.numpyutils import *
from CurveViz import graph
import sys

from wave1D_func1 import solver

def solver_r(I, f, c, U_L, L, n, dt, tstop,
             graphics=None, user_action=None, version='scalar'):
    """
    1D wave equation as in wave1D_func1.solver, but the present
    function models spherical waves. The physical solution is v=u/r.
    The boundary condition at r=0 is u=0 because of symmetry
    so u(L)=U_L is the only free boundary condition.

    This version is implemented as a wrapper of the solver
    in wave1D_func1.
    We compute v and display it through solver's user_action
    function before calling the supplied user's user_action
    function.
    """

    v = None  # bring v=u/r into play

    def U_0(t):
        return 0
    
    solutions = []  # store all u fields at all time levels
    
    def action_with_plot(u, x, t):

        v  = u.copy()        # get right length and type
        r = x                # radial coordinates
        v[1:] = u[1:]/r[1:]  # in-place modification
        v[0] = v[1]          # from the b.c. dv/dr=0 at r=0
        solutions.append(v.copy())
        if graphics is not None:
            graphics.configure(coor=x)
            graphics.plotcurve(v, legend='v(x,t=%9.4E)' % t, ps=0)
        if user_action is not None:
            user_action(v, x, t)  # call user's function with v

    dt, r, cpu = solver(I, f, c, U_0, U_L, L, n, dt, tstop,
                        action_with_plot, version)
    return dt, r, solutions, cpu



def test_radial_waves(plot=1, version='scalar'):
    L = 1
    c = 1
    def I(r): return 0
    def f(r,t):
        return 8*exp(-100*r)*sin(100*t)
    def U_L_func(t):
        return 0

    n = 500
    dr = L/float(n)
    dt = 0.5*dr
    tstop = 2
    if plot:
        g = graph(program='Gnuplot')
        g.configure(ymin=-0.055, ymax=0.055)
    else:
        g = None
    dt, r, v, cpu = solver_r(I, f, c, U_L_func, L, n, dt, tstop,
                             g, user_action=None, version=version)
    print 'CPU time', version, 'version:', cpu
    print 'v[0] final time:', v[-1][0]



def solver_r_v1(I, f, c, U_L, L, n, dt, tstop,
                version='scalar', plot=True, umin=0, umax=1):
    """
    1D wave equation as in wave1D_func1.solver, but the present
    function models spherical waves. The physical solution is v=u/r.
    The boundary condition at r=0 is u=0 because of symmetry
    so u(L)=U_L is the only free boundary condition.

    This version winds numerical solution and visualization
    together. An alternative is the solver_r function.
    """
    import time
    t0 = time.clock()
    
    dr = L/float(n)
    r = linspace(0, L, n+1)  # grid points in r dir
    if dt <= 0:  dt = dr/float(c)  # max time step? stability limit
    C2 = (c*dt/dr)**2        # help variable in the scheme
    dt2 = dt*dt

    up = zeros(n+1)   # solution array
    u  = up.copy()    # solution at t-dt
    um = up.copy()    # solution at t-2*dt
    v  = up.copy()    # physical solution (up/r)
    
    # set initial condition (pointwise - allows straight if-tests):
    for i in iseq(0,n):
        u[i] = I(r[i])
    for i in iseq(1,n-1):
        um[i] = u[i] + 0.5*C2*(u[i-1] - 2*u[i] + u[i+1]) + \
                dt2*f(x[i], t)
    um[0] = 0;  um[n] = U_L(t+dt)

    if plot:
        g = graph(program='Gnuplot')
        g.configure(ymin=umin, ymax=umax, coor=r)
        v[1:] = u[1:]/r[1:]
        v[0] = v[1]  # from the b.c. dv/dr=0 at r=0
        g.plotcurve(v, legend='u(r,t=0)')

    solutions = [u.copy()]  # hold all u arrays, start with init cond.
    t = 0.0
    while t <= tstop:
        t_old = t;  t += dt
        # update all inner points:
        if version == 'scalar':
            for i in iseq(start=1, stop=n-1):
                up[i] = - um[i] + 2*u[i] + \
                        C2*(u[i-1] - 2*u[i] + u[i+1]) + \
                        dt2*f(r[i], t_old)
        elif version == 'vectorized':
            up[1:n] = - um[1:n] + 2*u[1:n] + \
                     C2*(u[0:n-1] - 2*u[1:n] + u[2:n+1]) + \
                     dt2*f(r[1:n], t_old)
            
        # insert boundary conditions:
        up[0] = 0;  up[n] = U_L(t)
        # physical solution:
        v[1:] = up[1:]/r[1:]; v[0] = v[1] 
        # update data structures for next step
        um = u.copy(); u = up.copy() 
        # efficiency improvement of the update:
        # tmp = um; um = u; u = up; up = tmp
        if plot: g.plotcurve(v, legend='v(r,t=%9.4E)' % t)
        solutions.append(v.copy())  # save a copy!

    t1 = time.clock()
    return dt, r, solutions, t1-t0


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print """Usage %s test_radial_waves 1 "'vectorized'" """ % \
              sys.argv[0]
        sys.exit(0)
    cmd = '%s(%s)' % (sys.argv[1], ', '.join(sys.argv[2:]))
    print cmd
    exec(cmd)
