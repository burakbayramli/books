#!/usr/bin/env python
"""
Classes for solving a 1D wave equation.
"""
from __future__ import division  # disable integer division
from scitools.numpyutils import *
from CurveViz import graph
import sys

from scitools.PrmDictBase import PrmDictBase

class WaveEq1(PrmDictBase):
    def __init__(self, **kwargs):
        PrmDictBase.__init__(self)
        self.physical_prm = {
            'f': 0,
            'I': 1,
            'bc_0': 0,
            'bc_L': 0,
            'L': 1,
            'c': 1,
            }
        self.numerical_prm = {
            'dt': 0,
            'safety_factor': 1.0,  # multiplies dt
            'tstop': 1,
            'n': 10,
            'user_action': lambda s: None,  # callable with one arg.
            'scheme_coding': 'scalar',  # alt: 'vectorized'
            }
        # bring variables into existence (with dummy values):
        self.x  = zeros(1)         # grid points
        self.up = zeros(1)         # sol. at new time level
        self.u  = self.up.copy()   # prevous time level
        self.um = self.up.copy()   # two time levels behind

        self._prm_list = [self.physical_prm, self.numerical_prm]
        self._type_check = {'n': (int,float), 'tstop': (int,float),
                            'dt': (int,float),
                            'safety_factor': (int,float)}
        self.user_prm = None   # no extra user parameters
        self.set(**kwargs)     # assign parameters (if any kwargs)
        self.finished = False  # enables stopping simulations
        
    def _update(self):
        """Update internal data structures."""
        # this method is called by PrmDictBase.set
        P = self.physical_prm; N = self.numerical_prm # short forms
        # ensure that whatever the user has provided for I, f, etc.
        # we can call the quantity as a plain function of x:
        for funcname in 'I', 'f', 'bc_0', 'bc_L', 'c':
            P[funcname] = wrap2callable(P[funcname])
        dx = P['L']/float(N['n'])
        # update coordinates and solution arrays:
        if len(self.u) != N['n'] +1:
            self.x = seq(0, P['L'], dx)
            self.up = zeros(N['n']+1)
            self.u  = self.up.copy()
            self.um = self.up.copy()
        # stability limit: dt = dx/max(c)
        # (enable non-constant c(x,t) - subclasses need this)
        max_c = max([P['c'](x, 0) for x in self.x]) # loop is safest
        dt_limit = dx/max_c
        if N['dt'] <= 0 or N['dt'] > dt_limit:
            N['dt'] = N['safety_factor']*dt_limit

    def set_ic(self):
        """Set initial conditions."""
        self.t = 0.0  # initial time
        x, up, u, um, n, dx, dt, c, f, U_0, U_L = self.short_forms()
        I = self.physical_prm['I']
        for i in xrange(len(u)):
            u[i] = I(self.x[i])
        C2 = (dt/dx)**2
        for i in iseq(1,n-1):
            um[i] = u[i] + 0.5*C2*(u[i-1] - 2*u[i] + u[i+1]) + \
                    dt*dt*f(x[i], 0.0)
        um[0] = U_0(dt)
        um[n] = U_L(dt)
        # self.up is the unknown u function, let self.up=self.u
        # initially, as this is natural in the user_action func.
        self.up = u.copy()  

    def before_timeloop(self):
        return
    def after_timeloop(self):
        return
    
    def solve_problem(self):
        self.finished = False  # can be set by user, GUI, etc.
        import time
        t0 = time.clock()
        self.before_timeloop()
        self.numerical_prm['user_action'](self)
        
        while self.t < self.numerical_prm['tstop'] and not \
              self.finished:
            self.t += self.numerical_prm['dt']
            self.solve_at_this_time_step()
            self.numerical_prm['user_action'](self)
            self.um, self.u, self.up = self.u, self.up, self.um
            #self.um = self.u.copy(); self.u = self.up.copy()   # slow
        self.after_timeloop()
        t1 = time.clock()
        self.cpu = t1 - t0

    def short_forms(self):
        r = [self.x, self.up, self.u, self.um,
             self.numerical_prm['n'],
             self.x[1] - self.x[0],  # uniform grid cell size
             self.numerical_prm['dt']] + \
             [self.physical_prm[i] for i in \
                                  'c', 'f', 'bc_0', 'bc_L']
        return r
             
    def solve_at_this_time_step(self):
        x, up, u, um, n, dx, dt, c, f, U_0, U_L = self.short_forms()
        t = self.t; t_old = t - dt
        c = c(x[0]) # c is assumed constant in the scheme here
        C2 = (c*dt/dx)**2  # Courant number sequared
        if self.numerical_prm['scheme_coding'] == 'scalar':
            # update all inner points:
            for i in iseq(start=1, stop=n-1):
                up[i] = - um[i] + 2*u[i] + \
                       C2*(u[i-1] - 2*u[i] + u[i+1]) + \
                       dt*dt*f(x[i], t_old)
        elif self.numerical_prm['scheme_coding'] == 'vectorized':
            up[1:n] = - um[1:n] + 2*u[1:n] + \
                     C2*(u[0:n-1] - 2*u[1:n] + u[2:n+1]) + \
                     dt*dt*f(x[1:n], t_old)
        else:
            raise ValueError, 'version=%s' % version
        # insert boundary conditions:
        up[0] = U_0(t);  up[n] = U_L(t)


class WaveEq2(WaveEq1):
    """
    Wave equation with variable coefficient (wave velocity)
    and homogeneous Neumann boundary conditions.
    Main application area: long water waves.
    c=c(x,t), f=d^2c/dt^2.
    """
    def __init__(self):
        WaveEq1.__init__(self)
        self.physical_prm['f'] = self.d2c2dt2
        self.physical_prm['damping'] = 0 
    # should be able to disable some prm,
    # can take del here and update short_form,
    # or just disable setting: self.disabled_prm = ['bc_L','bc_0','f']

    def d2c2dt2(self, x, t):
        """Time-dependent source term (for water waves and moving bottom)."""
        c = self.physical_prm['c']
        eps = 1.0E-4
        return -(c(x,t+eps)**2-2*c(x,t)**2+c(x,t-eps)**2)/eps**2

    def set_ic(self):
        WaveEq1.set_ic(self)  # self.u is ok from this method
        x, up, u, um, n, dx, dt, c, f, dummy1, dummy2 = \
           self.short_forms()
        # self.um2 was not correctly computed in WaveEq1.set_ic
        C2 = (dt/dx)**2
        # turn c(x,t)**2 into array k:
        k = zeros(n+1)
        for i in range(len(x)):  # safest...
            k[i] = c(x[i], 0)**2
            
        for i in iseq(1, n-1):
            um[i] = u[i] + 0.5*C2*(
                     0.5*(k[i+1]+k[i])*(u[i+1] - u[i]) - \
                     0.5*(k[i]+k[i-1])*(u[i] - u[i-1])) + \
                     dt*dt*f(x[i], 0)
        i = 0; im1 = i+1; ip1 = i+1
        um[i] = u[i] + 0.5*C2*(
              0.5*(k[ip1]+k[i])*(u[ip1] - u[i]) - \
                     0.5*(k[i]+k[im1])*(u[i] - u[im1])) + \
                     dt*dt*f(x[i], 0)
        i = n; im1 = i-1; ip1 = i-1
        um[i] = u[i] + 0.5*C2*(
                     0.5*(k[ip1]+k[i])*(u[ip1] - u[i]) - \
                     0.5*(k[i]+k[im1])*(u[i] - u[im1])) + \
                     dt*dt*f(x[i], 0)
        
        
    def solve_at_this_time_step(self):
        x, up, u, um, n, dx, dt, c, f, dummy1, dummy2 = \
           self.short_forms()
        t = self.t; t_old = t - dt
        h = dt/dx**2
        C2 = (dt/dx)**2
        # turn function c**2 into array k
        k = zeros(n+1)
        for i in range(len(x)):  # safest...
            k[i] = c(x[i], t)**2

        if self.numerical_prm['scheme_coding'] == 'scalar':
            # update all inner points:
            for i in iseq(start=1, stop=n-1):
                up[i] = - um[i] + 2*u[i] + C2*(
                     0.5*(k[i+1]+k[i])*(u[i+1] - u[i]) - \
                     0.5*(k[i]+k[i-1])*(u[i] - u[i-1])) + \
                     dt*dt*f(x[i], t_old)
        elif self.numerical_prm['scheme_coding'] == 'vectorized':
            up[1:n] = - um[1:n] + 2*u[1:n] + C2*(
                     0.5*(k[2:n+1]+k[1:n])*(u[2:n+1] - u[1:n]) - \
                     0.5*(k[1:n]+k[0:n-1])*(u[1:n] - u[0:n-1])) + \
                     dt*dt*f(x[1:n], t_old)

        # insert boundary conditions:
        i = 0; im1 = i+1; ip1 = i+1
        up[i] = - um[i] + 2*u[i] + C2*(
               0.5*(k[ip1]+k[i])*(u[ip1] - u[i]) - \
               0.5*(k[i]+k[im1])*(u[i] - u[im1])) + \
               dt*dt*f(x[i], t_old)
        i = n; im1 = i-1; ip1 = i-1
        up[i] = - um[i] + 2*u[i] + C2*(
               0.5*(k[ip1]+k[i])*(u[ip1] - u[i]) - \
               0.5*(k[i]+k[im1])*(u[i] - u[im1])) + \
               dt*dt*f(x[i], t_old)


class SolverWithViz:
    def __init__(self, solver, plot=0, **graphics_kwargs):
        """Store solver instance. Initialize graphics tool."""
        self.s = solver
        self.solutions = []  # store self.up at each time level

        if not 'program' in graphics_kwargs:
            graphics_kwargs['program'] = 'Gnuplot'
        self._plot = plot
        if self._plot:
            self.g = graph(**graphics_kwargs)
            # could use scitools.easyviz.blt_ instead...
        else:
            self.g = None

    def set_graphics(self, ymin, ymax, xcoor):
        """Connect solver's grid to graphics tool, fix y axis."""
        if self.g is not None:
            self.g.configure(ymin=ymin, ymax=ymax, coor=xcoor)
            
    def do_graphics(self):
        if self.g is not None:
            self.g.plotcurve(self.s.up,
                 legend='u(x,t=%g)' % self.s.t,
                 ps= self._plot==2)
            
    def action(self, solver):
        self.do_graphics()
        self.solutions.append(self.s.up.copy())

# solve same problem as in test_solver_plug

def test_WaveEq1_plug(plot=1, version='scalar', n=50):
    """
    Use class SolverWithViz, which has a solver and
    some graphics object to do plotting.
    """
    L = 1
    c = 1
    tstop = 2
    def I(x):
        """Plug profile as initial condition."""
        if abs(x-L/2.0) > 0.1:
            return 0
        else:
            return 1

    w = SolverWithViz(WaveEq1(), plot=plot, program='Gnuplot')
    w.s.usage()
    w.s.set(I=I, f=0, bc_0=0, bc_L=0, c=1, n=n, tstop=9.95,
            user_action=w.action, scheme_coding=version)
    # when n is set, bind x to graphics:
    w.set_graphics(ymin=-1.1, ymax=1.1, xcoor=w.s.x)  # fix y axis
    w.s.set(f=lambda x,t: 0) # speeds up the code significantly (70%)
    w.s.dump()
    w.s.set_ic()
    w.s.solve_problem()
    print 'CPU time:', w.s.cpu
    if not allclose(w.solutions[0], w.solutions[-1],
                    atol=1.0E-10, rtol=1.0E-12):
        print 'error in computations'
        if plot:
            w.g.plotcurves([(w.solutions[0],'t=0'),
            (w.solutions[-1],'t=%g' % w.s.t)])
    else:
        print 'correct solution'

# no test for WaveEq2 yet, except in wave1D_GUI.py


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print """Usage %s test_WaveEq1_plug 1 "'vectorized'" """ % \
              sys.argv[0]
        sys.exit(0)
    cmd = '%s(%s)' % (sys.argv[1], ', '.join(sys.argv[2:]))
    print cmd
    exec(cmd)
