#!/usr/bin/env python
"""
Functions for solving a 1D wave equation using grid and
field objects.
"""
from __future__ import division  # disable integer division
from scitools.numpyutils import *
from CurveViz import *
from scitools.BoxGrid import *
from scitools.BoxField import BoxField

def solver(I, f, c, U_0, U_L, L, n, dt, tstop,
           user_action=None, version='scalar'):
    """
    Solve the wave equation u_tt=u_xx + f(x,t) on (0,L) with
    u(0,t)=U_0(t), u(L,t)=U_L(t), for t=dt,2*dt,...,tstop
    Initial conditions: u(x,0)=I(x), du/dt=0.

    n is the total number of grid cells; grid points are numbered
    from 0 to n.

    dt is the time step. If dt<=0, the optimal time step
    (dt=dx/c) is used.

    tstop is the stop time for the simulation.

    I, f, U_0, U_L are functions: I(x), f(x,t), U_0(t), U_L(t)

    user_action is a function of (u, x, t) where the calling code
    can add visualization, error computations, data analysis,
    store solutions, etc.
    """
    import time
    t0 = time.clock()

    g = UniformBoxGrid(x=(0,L), nx=n)

    up_f = BoxField(g, 'up')
    u_f  = BoxField(g, 'u')
    um_f = BoxField(g, 'um')

    # short hand notation:
    dx = g.dx
    x = g.xcoor
    up = up_f.values
    u = u_f.values
    um = um_f.values

    if dt <= 0:  dt = dx/float(c)  # max time step?
    C2 = (c*dt/dx)**2        # help variable in the scheme
    dt2 = dt*dt

    # set initial condition (pointwise - allows straight if-tests):
    t = 0.0
    for i in iseq(0,n):
        u[i] = I(x[i])
    for i in iseq(1,n-1):
        um[i] = u[i] + 0.5*C2*(u[i-1] - 2*u[i] + u[i+1]) + \
                dt2*f(x[i], t)
    um[0] = U_0(t+dt);  um[n] = U_L(t+dt)

    if user_action is not None:
        user_action(u, x, t)

    while t <= tstop:
        t_old = t;  t += dt
        # update all inner points:
        if version == 'scalar':
            for i in iseq(start=1, stop=n-1):
                up[i] = - um[i] + 2*u[i] + \
                        C2*(u[i-1] - 2*u[i] + u[i+1]) + \
                        dt2*f(x[i], t_old)
        elif version == 'vectorized':
            up[1:n] = - um[1:n] + 2*u[1:n] + \
                      C2*(u[0:n-1] - 2*u[1:n] + u[2:n+1]) + \
                      dt2*f(x[1:n], t_old)
        else:
            raise ValueError, 'version=%s' % version
            
        # insert boundary conditions:
        up[0] = U_0(t);  up[n] = U_L(t)
        if user_action is not None:
            user_action(up, x, t)

        # update data structures for next step:
        um, u, up = u, up, um                # switch references

    t1 = time.clock()
    return dt, x, t1-t0


def visualizer(I, f, c, U_0, U_L, L, n, dt, tstop,
               user_action=None, version='scalar', graphics=None):
    """
    Call solver but let the user_action funtion be a function
    where the solution is visualized and stored in a list.
    All arguments are passed on to the solver function,
    except graphics. graphics is a plot object with the max/min
    values of the y axis set in the calling code.
    """
    solutions = []     # store all u fields at all time levels

    def action_with_plot(u, x, t):
        # note: nested function blocks may lead to
        # mixing of scopes of variables - this might be tricky

        if graphics is not None:
            graphics.configure(coor=x)
            graphics.plotcurve(u, legend='u(x,t=%9.4E)' % t, ps=0)
            
        solutions.append(u.copy())  # save a copy!
        if user_action is not None:
            user_action(u, x, t)  # call user's function

    dt, x, cpu = solver(I, f, c, U_0, U_L, L, n, dt, tstop,
                        action_with_plot, version)
    return solutions, x, dt, cpu


def test_solver_plug(plot=1, version='scalar', n=50):
    L = 1
    c = 1
    tstop = 2
    def I(x):
        """Plug profile as initial condition."""
        if abs(x-L/2.0) > 0.1:
            return 0
        else:
            return 1

    def f(x,t):
        return 0

    def U_0(t):
        return 0

    def U_L(t):
        return 0

    def action(u, x, t):
        pass
        #print t, u
        
    if plot:
        g = graph(program='Gnuplot')
        g.configure(ymin=-1.1, ymax=1.1)
    else:
        g = None

    import time
    t0 = time.clock()
    solutions, x, dt, cpu = visualizer(I, f, c, U_0, U_L, L,
    n, 0, tstop, user_action=None, version=version, graphics=g)

    print 'CPU time: %s version =' % version, cpu
    # check that first and last (if tstop=2) are equal:
    if not allclose(solutions[0], solutions[-1],
                    atol=1.0E-10, rtol=1.0E-12):
        print 'error in computations'
    else:
        print 'correct solution'


if __name__ == '__main__':
    test_solver_plug(plot=1, version='scalar', n=50)
