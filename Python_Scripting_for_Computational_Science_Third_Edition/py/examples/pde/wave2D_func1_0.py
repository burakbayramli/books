#!/usr/bin/env python
"""
As wave2D_func1.py, but the wave2D_func1_loop_0 module is used
for the purpose of measuring overhead in array copying.
"""

from __future__ import division  # disable integer division
from scitools.numpyutils import *
from scitools.misc import system
import sys, Gnuplot

    
def solver(I, f, c, bc, Lx, Ly, nx, ny, dt, tstop,
           user_action=None, 
           implementation={'ic': 'vectorized',  # or 'scalar'
                           'inner': 'vectorized',
                           'bc': 'vectorized',
                           'storage': 'f77'},
           verbose=True):
    """
    Solve the 2D wave equation u_tt = u_xx + u_yy + f(x,t) on (0,L) with
    u = bc(x,y, t) on the boundary and initial condition du/dt = 0.

    nx and ny are the total number of grid cells in the x and y
    directions. The grid points are numbered as (0,0), (1,0), (2,0),
    ..., (nx,0), (0,1), (1,1), ..., (nx, ny).

    dt is the time step. If dt<=0, an optimal time step is used.
    tstop is the stop time for the simulation.

    I, f, bc are functions: I(x,y), f(x,y,t), bc(x,y,t)

    user_action: function of (u, x, y, t) called at each time
    level (x and y are one-dimensional coordinate vectors).
    This function allows the calling code to plot the solution,
    compute errors, etc.

    implementation: a dictionary specifying how the initial
    condition ('ic'), the scheme over inner points ('inner'),
    and the boundary conditions ('bc') are to be implemented.
    Two values are legal: 'scalar' or 'vectorized'.
    'scalar' means straight loops over grid points, while
    'vectorized' means special NumPy vectorized operations.
    If a key in the implementation dictionary is missing, it
    defaults in this function to 'scalar' (the safest strategy).
    Note that if 'vectorized' is specified, the functions I, f,
    and bc must work in vectorized mode. It is always recommended
    to first run the 'scalar' mode and then compare 'vectorized'
    results with the 'scalar' results to check that I, f, and bc
    work.

    verbose: true if a message at each time step is written,
    false implies no output during the simulation.
    """
    dx = Lx/float(nx)
    dy = Ly/float(ny)
    x = linspace(0, Lx, nx+1)  # grid points in x dir
    y = linspace(0, Ly, ny+1)  # grid points in y dir
    xv = x[:,newaxis]          # for vectorized function evaluations
    yv = y[newaxis,:]
    if dt <= 0:                # max time step?
        dt = (1/float(c))*(1/sqrt(1/dx**2 + 1/dy**2))
    Cx2 = (c*dt/dx)**2;  Cy2 = (c*dt/dy)**2    # help variables
    dt2 = dt**2

    up = zeros((nx+1,ny+1))    # solution array
    u  = up.copy()             # solution at t-dt
    um = up.copy()             # solution at t-2*dt

    # use scalar implementation mode if no info from user:
    if 'ic' not in implementation:
        implementation['ic'] = 'scalar'
    if 'bc' not in implementation:
        implementation['bc'] = 'scalar'
    if 'inner' not in implementation:
        implementation['inner'] = 'scalar'
    if 'f77' in implementation.itervalues():
        # import F77 extension module, or build it if necessary
        try:
            import wave2D_func1_loop as f77
        except:
            print 'Make the F77 extension module on the fly...'
            cmd = 'f2py -m wave2D_func1_loop -c --build-dir tmp1 '\
                  '-DF2PY_REPORT_ON_ARRAY_COPY=1 wave2D_func1_loop_0.f'
            failure, output = system(cmd)
            try:
                import wave2D_func1_loop as f77
            except:
                print 'Something went wrong with f2py - '\
                      'try manual executition of\n  ', cmd
                print output
                sys.exit(1)
        # turn arrays to column major storage after the init. cond.
                        
    # set initial condition:
    t = 0.0
    if implementation['ic'] == 'scalar':
        for i in iseq(0,nx):
            for j in iseq(0,ny):
                u[i,j] = I(x[i], y[j])
        for i in iseq(1,nx-1):
            for j in iseq(1,ny-1):
                um[i,j] = u[i,j] + \
                  0.5*Cx2*(u[i-1,j] - 2*u[i,j] + u[i+1,j]) + \
                  0.5*Cy2*(u[i,j-1] - 2*u[i,j] + u[i,j+1]) + \
                  dt2*f(x[i], y[j], t) 
        # boundary values of um (equals t=dt when du/dt=0)
        i = 0
        for j in iseq(0,ny): um[i,j] = bc(x[i], y[j], t+dt)
        j = 0
        for i in iseq(0,nx): um[i,j] = bc(x[i], y[j], t+dt)
        i = nx
        for j in iseq(0,ny): um[i,j] = bc(x[i], y[j], t+dt)
        j = ny
        for i in iseq(0,nx): um[i,j] = bc(x[i], y[j], t+dt)
    elif implementation['ic'] == 'vectorized' or \
         implementation['ic'] == 'f77':  # not impl. in F77
        # vectorized version:
        u = I(xv,yv)
        um[1:nx,1:ny] = u[1:nx,1:ny] + \
        0.5*Cx2*(u[0:nx-1,1:ny] - 2*u[1:nx,1:ny] + u[2:nx+1,1:ny]) + \
        0.5*Cy2*(u[1:nx,0:ny-1] - 2*u[1:nx,1:ny] + u[1:nx,2:ny+1]) + \
        dt2*f(xv[1:nx,:], yv[:,1:ny], 0.0)
        # boundary values (t=dt):
        i = 0;  um[i,:] = bc(x[i], y, t+dt)
        j = 0;  um[:,j] = bc(x, y[j], t+dt)
        i = nx; um[i,:] = bc(x[i], y, t+dt)
        j = ny; um[:,j] = bc(x, y[j], t+dt)

    if user_action is not None:
        user_action(u, x, y, t)  # allow user to plot etc.
    
    while t <= tstop:
        t_old = t;  t += dt
        if verbose:
            print 'solving (%s version) at t=%g' % \
                  (implementation['inner'], t)
        # update all inner points:
        if implementation['inner'] == 'scalar':
            for i in iseq(start=1, stop=nx-1):
                for j in iseq(start=1, stop=ny-1):
                    up[i,j] = - um[i,j] + 2*u[i,j] + \
                       Cx2*(u[i-1,j] - 2*u[i,j] + u[i+1,j]) + \
                       Cy2*(u[i,j-1] - 2*u[i,j] + u[i,j+1]) + \
                       dt2*f(x[i], y[j], t_old)
        elif implementation['inner'] == 'vectorized':
            up[1:nx,1:ny] = - um[1:nx,1:ny] + 2*u[1:nx,1:ny] + \
           Cx2*(u[0:nx-1,1:ny] - 2*u[1:nx,1:ny] + u[2:nx+1,1:ny]) + \
           Cy2*(u[1:nx,0:ny-1] - 2*u[1:nx,1:ny] + u[1:nx,2:ny+1]) + \
           dt2*f(xv[1:nx,:], yv[:,1:ny], t_old)
        elif implementation['inner'] == 'f77':
            f_array = f(xv, yv, t_old)
            if isinstance(f_array, (float,int)):
                # f was not properly vectorized, turn const into array:
                f_array = zeros((x.size,y.size)) + f_array
            up = f77.loop(u, um, f_array, Cx2, Cy2, dt2)

            #id_u = id(u); id_um = id(um)
            #up,u,um = f77.loop(up, u, um, f_array, Cx2, Cy2, dt2)
            #print 'u changed:', id_u!=id(u),
            #print 'um changed:', id_um!=id(um),
        else:
            raise ValueError, 'version=%s' % implementation['inner']

        # insert boundary conditions:
        if implementation['bc'] == 'scalar':
            i = 0
            for j in iseq(0,ny): up[i,j] = bc(x[i], y[j], t)
            j = 0
            for i in iseq(0,nx): up[i,j] = bc(x[i], y[j], t)
            i = nx
            for j in iseq(0,ny): up[i,j] = bc(x[i], y[j], t)
            j = ny
            for i in iseq(0,nx): up[i,j] = bc(x[i], y[j], t)
        elif implementation['bc'] == 'vectorized' or \
             implementation['ic'] == 'f77':  # not impl. in F77
            i = 0;  up[i,:] = bc(x[i], y, t)
            j = 0;  up[:,j] = bc(x, y[j], t)
            i = nx; up[i,:] = bc(x[i], y, t)
            j = ny; up[:,j] = bc(x, y[j], t)

        if user_action is not None:
            user_action(up, x, y, t)
        # update data structures for next step:
        um, u, up = u, up, um
        #tmp = um; um = u; u = up; up = tmp
        # safer and slower: um = u.copy(); u = up.copy()

    return dt  # dt might be computed in this function



def solver0(I, f, c, bc, Lx, Ly, nx, ny, dt, tstop,
           user_action=None):
    """
    As solver, but only scalar mode implementation of loops.
    """
    dx = Lx/float(nx)
    dy = Ly/float(ny)
    x = linspace(0, Lx, nx+1)  # grid points in x dir
    y = linspace(0, Ly, ny+1)  # grid points in y dir
    if dt <= 0:                # max time step?
        dt = (1/float(c))*(1/sqrt(1/dx**2 + 1/dy**2))
    Cx2 = (c*dt/dx)**2;  Cy2 = (c*dt/dy)**2    # help variables
    dt2 = dt**2

    up = zeros((nx+1,ny+1))    # solution array
    u  = up.copy()             # solution at t-dt
    um = up.copy()             # solution at t-2*dt

    # set initial condition:
    t = 0.0
    for i in iseq(0,nx):
        for j in iseq(0,ny):
            u[i,j] = I(x[i], y[j])
    for i in iseq(1,nx-1):
        for j in iseq(1,ny-1):
            um[i,j] = u[i,j] + \
              0.5*Cx2*(u[i-1,j] - 2*u[i,j] + u[i+1,j]) + \
              0.5*Cy2*(u[i,j-1] - 2*u[i,j] + u[i,j+1]) + \
              dt2*f(x[i], y[j], t) 
    # boundary values of um (equals t=dt when du/dt=0)
    i = 0
    for j in iseq(0,ny): um[i,j] = bc(x[i], y[j], t+dt)
    j = 0
    for i in iseq(0,nx): um[i,j] = bc(x[i], y[j], t+dt)
    i = nx
    for j in iseq(0,ny): um[i,j] = bc(x[i], y[j], t+dt)
    j = ny
    for i in iseq(0,nx): um[i,j] = bc(x[i], y[j], t+dt)

    if user_action is not None:
        user_action(u, x, y, t)  # allow user to plot etc.
    
    while t <= tstop:
        t_old = t;  t += dt

        # update all inner points:
        for i in iseq(start=1, stop=nx-1):
            for j in iseq(start=1, stop=ny-1):
                up[i,j] = - um[i,j] + 2*u[i,j] + \
                     Cx2*(u[i-1,j] - 2*u[i,j] + u[i+1,j]) + \
                     Cy2*(u[i,j-1] - 2*u[i,j] + u[i,j+1]) + \
                     dt2*f(x[i], y[j], t_old)

        # insert boundary conditions:
        i = 0
        for j in iseq(0,ny): up[i,j] = bc(x[i], y[j], t)
        j = 0
        for i in iseq(0,nx): up[i,j] = bc(x[i], y[j], t)
        i = nx
        for j in iseq(0,ny): up[i,j] = bc(x[i], y[j], t)
        j = ny
        for i in iseq(0,nx): up[i,j] = bc(x[i], y[j], t)

        if user_action is not None:
            user_action(up, x, y, t)
        
        um, u, up = u, up, um  # update data structures
    return dt  # dt might be computed in this function


def verify_implementations(I, f, c, bc, Lx, Ly, nx, ny, tstop):
    """Run scalar, vectorized, and f77 mode and compare results."""

    class Action:
        def __init__(self):
            self.solutions = {}
        def init(self, version):
            """Init list for solutions."""
            self._version = version
            self.solutions[self._version] = []
        def __call__(self, u, x, y, t):
            # takes time...
            self.solutions[self._version].append(u.copy())

    action = Action()
    versions = ('scalar', 'vectorized', 'f77')
    implementation = {}
    for version in versions:
        for key in 'ic', 'inner', 'bc':
            implementation[key] = version
        action.init(version)
        print version
        dt = solver(I, f, c, bc, Lx, Ly, nx, ny, 0, tstop,
                    action, implementation)
    # compare solutions:
    ref = action.solutions[versions[0]]
    for version in versions[1:]:
        sol = action.solutions[version]
        # compare sol with ref:
        for timestep in range(len(sol)):
            error = ref[timestep] - sol[timestep]
            error_measure = sqrt(dot(error.flat, error.flat))
            if error_measure < 1.0E-14:
                error_measure = 0.0
            print "'%s' compared with '%s' at timestep "\
                  "%02d: difference=%g" % \
                  (versions[0],  version, timestep, error_measure)
        
def test_verify():
    Lx = 10;  Ly = 10;  c = 1.0

    def I(x, y):
        return exp(-(x-Lx/2.0)**2/2.0 -(y-Ly/2.0)**2/2.0)
    def f(x, y, t):
        return sin(2*x) + y
    def bc(x, y, t):
        return sin(t)

    #nx = 15;  ny = 10; tstop = 2
    nx = 4;  ny = 3; tstop = 16
    verify_implementations(I, f, c, bc, Lx, Ly, nx, ny, tstop)
    

def test_plot1(plot=1, version='scalar'):
    """
    Initial Gaussian bell in the middle of the domain.
    plot: not plot: 0; on the screen: 1, hardcopy too: 2
    """
    Lx = 10
    Ly = 10
    c = 1.0

    def I2(x, y):
        return exp(-(x-Lx/2.0)**2/2.0 -(y-Ly/2.0)**2/2.0)
    def f(x, y, t):
        return 0.0
    def bc(x, y, t):
        return 0.0

    if plot:
        g = Gnuplot.Gnuplot(persist=1)
        g('set parametric')
        g('set data style lines')
        g('set hidden')
        g('set contour base')
        g('set zrange [-0.7:0.7]') # nice plot...
        
    import time

    def action(u, x, y, t):
        #print 'action, t=',t,'\nu=',u, '\nx=',x, '\ny=', y
        if plot:
            data = Gnuplot.GridData(u, x, y, binary=0)
            g.splot(data)
            g('set title "t=%g"' % t)
            if plot == 2:
                g.hardcopy(filename='tmp_%020f.ps' % t, enhanced=1, mode='eps',
                           color=0, fontname='Times-Roman', fontsize=14)
                time.sleep(1)
            time.sleep(0.2) # pause between frames

    t0 = time.clock()
    implementation = {'ic': version, 'inner': version, 'bc': version}
    nx = 40; ny = 40; tstop = 700
    dt = solver(I2, f, c, bc, Lx, Ly, nx, ny, 0, tstop,
                user_action=action, implementation=implementation)
    t1 = time.clock()
    cpu = t1 - t0
    print 'CPU time: %s version =' % version, cpu
    time.sleep(3)
    return cpu


def test_plot2(plot=1, version='scalar'):
    """
    As test_plot1, but the action function is a class.
    """
    Lx = 10
    Ly = 10
    c = 1.0

    def I2(x, y):
        return exp(-(x-Lx/2.0)**2/2.0 -(y-Ly/2.0)**2/2.0)
    def f(x, y, t):
        return 0.0
    def bc(x, y, t):
        return 0.0

    class Visualizer:
        def __init__(self, plot=0):
            self.plot = plot
            if self.plot:
                self.g = Gnuplot.Gnuplot(persist=1)
                self.g('set parametric')
                self.g('set data style lines')
                self.g('set hidden')
                self.g('set contour base')
                self.g('set zrange [-0.7:0.7]') # nice plot...

        def __call__(self, u, x, y, t):
            if self.plot:
                data = Gnuplot.GridData(u, x, y, binary=0)
                self.g.splot(data)
                self.g('set title "t=%g"' % t)
            if self.plot == 2:
                self.g.hardcopy(filename='tmp_%020f.ps' % t,
                                enhanced=1, mode='eps', fontsize=14,
                                color=0, fontname='Times-Roman')
                time.sleep(0.8)   # pause to finish plot
    import time
    t0 = time.clock()
    viz = Visualizer(plot)
    implementation = {'ic': version, 'inner': version, 'bc': version}
    nx = 40; ny = 40; tstop = 700
    dt = solver(I2, f, c, bc, Lx, Ly, nx, ny, 0, tstop,
                user_action=viz, implementation=implementation)
    t1 = time.clock()
    cpu = t1 - t0
    print 'CPU time: %s version =' % version, cpu
    time.sleep(3)
    return cpu


def test_error1(version='scalar'):
    """Analytical solution (bc=u)."""
    Lx = 10
    Ly = 10
    c = 1.0

    def exact(x, y, t):
        kx = pi/Lx; ky = pi/Ly; omega = sqrt(kx*kx + ky*ky)
        return cos(omega*t)*sin(kx*x)*sin(ky*y)

    def I1(x, ):
        return exact(x, y, 0)

    def bc(x, y, t):
        return exact(x, y, t)

    def I2(x, y):
        """Plug, but this is of no help since u=0 on the boundary..."""
        plug_length = 0.1
        if isinstance(x, NumPyArray) and isinstance(y, NumPyArray):
            # vectorized version is not impl.
            u = zeros((x.shape[0], y.shape[1]))
            # loop version:
            for i in range(len(x)):
                if x[i] < Lx/2.0 - plug_length or \
                   x[i] > Lx/2.0 + plug_length:
                    u[i,:] = 0.0
                else:
                    u[i,:] = 1.0
        else:
            # x and y are floats (assumed)
            if x < Lx/2.0 - plug_length or \
               x > Lx/2.0 + plug_length:
                u = 0.0
            else:
                u = 1.0
        return u
    
    def f(x, y, t):
        return 0.0
    
    import time

    error = []
    def action(u, x, y, t):
        e = exact(x, y, t) - u
        #error.append((t, innerproduct(e.flat,e.flat)))
        error.append(innerproduct(e.flat,e.flat))

    t0 = time.clock()
    implementation = {'ic': version, 'inner': version, 'bc': version}
    nx = 10; ny = 4; tstop = 20
    dt = solver(I1, f, c, bc, Lx, Ly, nx, ny, 0, tstop,
                user_action=action, implementation=implementation)
    t1 = time.clock()
    cpu = t1-t0
    print 'CPU time: %s version =' % version, cpu
    print 'max error:', max(error), 'min error:', min(error)
    time.sleep(3), max(error), min(error)
    return cpu

def benchmark(nx, tstop):
    """Initial Gaussian bell in the middle of the domain."""
    Lx = 10
    Ly = 10
    c = 1.0
    ny = nx

    def I2(x, y):
        return exp(-(x-Lx/2.0)**2/2.0 -(y-Ly/2.0)**2/2.0)
    def f(x, y, t):
        return 0.0
    def BC(x, y, t):
        return 0.0
    import time

    def action(u, x, y, t):
        #print t
        pass

    implementation = {}
    cpu = []
    fastest = 0
    for ic in 'f77', 'vectorized', 'scalar':
        for bc in 'f77', 'vectorized', 'scalar':
            for inner in 'f77', 'vectorized', 'scalar':
                implementation['ic'] = ic
                implementation['inner'] = inner
                implementation['bc'] = bc
                t0 = time.clock()
                dt = solver(I2, f, c, BC, Lx, Ly, nx, ny, 0, tstop,
                            user_action=None,
                            implementation=implementation,
                            verbose=False)
                t1 = time.clock()
                cpu.append((implementation, t1 - t0))
                if fastest == 0:
                    fastest = t1-t0  # fastest (f77) CPU time
                    print 'time step:', dt, 'means', tstop/dt, 'steps'
                    print 'fastest execution: %g seconds' % fastest
                print 'ic: %-10s bc: %-10s inner: %-10s  cpu: %g' % \
                      (implementation['ic'],implementation['bc'],
                       implementation['inner'],cpu[-1][1]/fastest)
    return cpu
    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print """Usage %s test_plot1 "'vectorized'" 1""" % \
              sys.argv[0]
        sys.exit(0)
    cmd = '%s(%s)' % (sys.argv[1], ', '.join(sys.argv[2:]))
    print cmd
    exec(cmd)
