#!/usr/bin/env python
"""
Functions for solving a 1D wave equation.
"""
from __future__ import division  # disable integer division
from scitools.numpyutils import *
from CurveViz import *

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
    
    dx = L/float(n)
    x = linspace(0, L, n+1)   # grid points in x dir
    if dt <= 0:  dt = dx/float(c)  # max time step?
    C2 = (c*dt/dx)**2         # help variable in the scheme
    dt2 = dt*dt

    up = zeros(n+1)   # NumPy solution array
    u  = up.copy()    # solution at t-dt
    um = up.copy()    # solution at t-2*dt
    
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
        #tmp = um; um = u; u = up; up = tmp  # traditional
        #um = u.copy(); u = up.copy()        # slow

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

def test_solver1(N, version='scalar'):
    """
    Very simple test case.
    Store the solution at every N time level.
    """
    def I(x):  return sin(2*x*pi/L)
    def f(x,t): return 0
    solutions = []
    # Need time_level_counter as global variable since
    # it is assigned in the action function (that makes
    # a variable local to that block otherwise).
    # The manager class below provides a cleaner solution.
    global time_level_counter
    time_level_counter = 0

    def action(u, x, t):
        global time_level_counter
        if time_level_counter % N == 0:
            solutions.append(u.copy())
        time_level_counter += 1

    n = 100; tstop = 6; L = 10
    dt, x, cpu = solver(I, f, 1.0, lambda t: 0, lambda t: 0,
                        L, n, 0, tstop,
                        user_action=action, version=version)
    print 'CPU time:', cpu
    print 'Max value in final u:', arrmax(solutions[-1])


class StoreSolution:
    """
    Very simple test case.
    Store the solution at every N time level.
    """
    def __init__(self):
        self.L = 10
        
    def I(self, x):     return sin(2*x*pi/self.L)
    def f(self, x, t):  return 0

    def action(self, u, x, t):
        if self.time_level_counter % self.N == 0:
            self.solutions.append(u.copy())
        self.time_level_counter += 1

    def main(self, N=1, version='scalar'):
        self.solutions = []
        self.time_level_counter = 0
        self.N = N
        n = 6; tstop = 40
        self.dt, self.x, self.cpu = \
           solver(self.I, self.f, 1.0, lambda t: 0, lambda t: 0,
                  self.L, n, 0, tstop,
                  user_action=self.action, version=version)



def test_solver2(N, plot=True, version='scalar'):
    s = StoreSolution()
    s.main(N, version)
    print 'CPU time:', s.cpu
    if len(s.x) < 10: print s.solutions
    if plot:
        from CurveViz import graph
        g = graph(program='Gnuplot', coor=s.x, ymax=1, ymin=-1)
        for s in s.solutions:
            g.plotcurve(s)
        


def test_solver1c(N, version='scalar'):
    """
    As test_solver1, but use class for action function.
    """
    def I(x):    return sin(2*x*pi/L)
    def f(x, t): return 0

    class Action:
        def __init__(self):
            self.solutions = []
            self.time_level_counter = 0

        def __call__(self, u, x, t):
            if self.time_level_counter % N == 0:
                self.solutions.append(u.copy())
            self.time_level_counter += 1

    action = Action()
    n = 100; tstop = 6; L = 10
    dt, x, cpu = solver(I, f, 1.0, lambda t: 0, lambda t: 0,
                        L, n, 0, tstop,
                        user_action=action, version=version)
    print 'CPU time:', cpu
    print 'Max value in final u:', arrmax(action.solutions[-1])


class ExactSolution1:
    def __init__(self):
        self.L = 10
        
    def exact(self, x, t):
        m = 3.0
        return cos(m*pi/self.L*t)*sin(m*pi/self.L*x)
        
    def I(self, x):     return self.exact(x, 0)
    def f(self, x, t):  return 0
    def U_0(self, t):   return self.exact(0, t)
    def U_L(self, t):   return self.exact(self.L, t)

    def action(self, u, x, t):
        e = u - self.exact(x, t)
        self.errors.append(sqrt(dot(e,e)))   # store norm of e

    def main(self, n, version='scalar'):
        self.errors = []
        tstop = 10
        self.dt, self.x, self.cpu = \
           solver(self.I, self.f, 1.0, self.U_0,
                  lambda t: self.exact(self.L, t),
                  self.L, n, 0, tstop,
                  user_action=self.action, version=version)

def test_solver3(version='scalar'):
    s = ExactSolution1()
    s.main(5, version)
    print 'Max error:', max(s.errors)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print """Usage %s test_solver_plug 1 "'vectorized'" """ % \
              sys.argv[0]
        sys.exit(0)
    cmd = '%s(%s)' % (sys.argv[1], ', '.join(sys.argv[2:]))
    print cmd
    exec(cmd)
