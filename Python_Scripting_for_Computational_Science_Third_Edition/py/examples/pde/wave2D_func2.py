#!/usr/bin/env python
"""
As wave2D_func1.py, but the code is divided more into functions
and classes.
"""

from __future__ import division  # disable integer division
from scitools.numpyutils import *
from scitools.misc import system
from scitools.StringFunction import StringFunction
import Gnuplot, time, sys, os
try:
    import weave  # SciPy 0.3
except ImportError:
    try:  # SciPy 0.4
        from scipy import weave
    except:
        raise ImportError, 'weave not found'
        pass # code will break later

def make_f77(f, bc, I):
    code = """
      subroutine scheme_f77(up, u, um, x, y, t, nx, ny, Cx2, Cy2,
     &                      dt2, t_old)
      integer nx, ny
      real*8 up(0:nx, 0:ny), u(0:nx, 0:ny), um(0:nx, 0:ny)
      real*8 x(0:nx), y(0:ny)
      real*8 Cx2, Cy2, dt2, t, t_old
Cf2py intent(in, out) up
      real*8 f
      external f

      do j = 1, ny-1
         do i = 1, nx-1
            up(i,j) = - um(i,j) + 2*u(i,j) + 
     &          Cx2*(u(i-1,j) - 2*u(i,j) + u(i+1,j)) +
     &          Cy2*(u(i,j-1) - 2*u(i,j) + u(i,j+1)) +
     &          dt2*f(x(i),y(j),t_old)
         end do
      end do
      return
      end

      subroutine ic_f77(u, um, x, y, nx, ny, Cx2, Cy2, dt)
      integer nx, ny
      real*8 u(0:nx, 0:ny), um(0:nx, 0:ny)
      real*8 x(0:nx), y(0:ny)
      real*8 Cx2, Cy2, dt, dt2
Cf2py intent(in, out) u, um
      real*8 f, ic
      external f, ic

      dt2 = dt*dt
      do j = 0, ny
         do i = 0, nx
            u(i,j) = ic(x(i),y(j))
         end do
      end do
      do j = 1, ny-1
         do i = 1, nx-1
            um(i,j) = u(i,j) +
     &          0.5*Cx2*(u(i-1,j) - 2*u(i,j) + u(i+1,j)) +
     &          0.5*Cy2*(u(i,j-1) - 2*u(i,j) + u(i,j+1)) +
     &          dt2*f(x(i),y(j),0.0D0)
         end do
      end do
C     boundary values:
      call bc_f77(um, x, y, nx, ny, dt)
      return
      end

      subroutine bc_f77(up, x, y, nx, ny, t)
      integer nx, ny
      real*8 up(0:nx, 0:ny)
      real*8 x(0:nx), y(0:ny)
      real*8 t
Cf2py intent(in, out) up
      real*8 bc
      external bc

      i = 0
      do j = 0, ny
         up(i,j) = bc(x(i),y(j),t)
      end do
      j = 0
      do i = 0, nx
         up(i,j) = bc(x(i),y(j),t)
      end do
      i = nx
      do j = 0, ny
         up(i,j) = bc(x(i),y(j),t)
      end do
      j = ny
      do i = 0, nx
         up(i,j) = bc(x(i),y(j),t)
      end do
      return
      end


%s

%s

%s

%s      
""" % (f.F77_code('f'), bc.F77_code('bc'), I.F77_code('ic'), I.F77_pow())

    f = open('_tmp.f', 'w')
    f.write(code)
    f.close()

    cmd = "f2py -m f77 -c --fcompiler='gfortran' --build-dir tmp2"\
          " -DF2PY_REPORT_ON_ARRAY_COPY=1 _tmp.f"
    print cmd
    os.system('rm -rf tmp2')
    failure, output = system(cmd)
    if failure:
        print 'unsuccessful F77 extension module compilation'
        print output
        sys.exit(1)

def ic_weave(u, um, I, f, bc, x, y, t, Cx2, Cy2, dt):
    nx, ny = len(x)-1, len(y)-1
    dt2 = dt*dt
    # additional code with f, bc, and I functions:
    extra_code = f.C_code('_f', inline=True) + \
                 bc.C_code('_bc', inline=True) + \
                 I.C_code('_I', inline=True)

    code = """
int i,j;
for (i=0; i<=nx; i++) {
  for (j=0; j<=ny; j++) {
    u(i,j) = _I(x(i), y(j));
  }
}
for (i=1; i<=nx-1; i++) {
  for (j=1; j<=ny-1; j++) {
    um(i,j) = u(i,j) + \
      0.5*Cx2*(u(i-1,j) - 2*u(i,j) + u(i+1,j)) + \
      0.5*Cy2*(u(i,j-1) - 2*u(i,j) + u(i,j+1)) + \
      dt2*_f(x(i), y(j), t);
  }
}
// boundary values of um (equals t=dt when du/dt=0)
double xv, yv;
i = 0; xv = x(i);
for (j=0; j<=ny; j++) { um(i,j) = _bc(xv, y(j), t+dt); }
j = 0; yv = y(j);
for (i=0; i<=nx; i++) { um(i,j) = _bc(x(i), yv, t+dt); }
i = nx; xv = x(i);
for (j=0; j<=ny; j++) { um(i,j) = _bc(xv, y(j), t+dt); }
j = ny; yv = y(j);
for (i=0; i<=nx; i++) { um(i,j) = _bc(x(i), yv, t+dt); }
"""
    args = ['u', 'um', 'nx', 'ny', 'x', 'y',
            'Cx2', 'Cy2', 'dt2', 'dt', 't']
    err = weave.inline(code, args,
                       type_converters=weave.converters.blitz,
                       support_code=extra_code, compiler='gcc')
    return u, um

    
def scheme_weave(up, u, um, f, bc, x, y, t, Cx2, Cy2, dt2, t_old):
    nx, ny = len(x)-1, len(y)-1
    extra_code = f.C_code('_f', inline=True) + \
                 bc.C_code('_bc', inline=True) + \
                 I.C_code('_I', inline=True)
    code = """
int i,j;
for (i=1; i<=nx-1; i++) {
  for (j=1; j<=ny-1; j++) {
    up(i,j) = -um(i,j) + 2*u(i,j) + \
      Cx2*(u(i-1,j) - 2*u(i,j) + u(i+1,j)) + \
      Cy2*(u(i,j-1) - 2*u(i,j) + u(i,j+1)) + \
      dt2*_f(x(i), y(j), t_old);
  }
}
"""
    args = ['up', 'u', 'um', 'nx', 'ny', 'x', 'y',
            'Cx2', 'Cy2', 'dt2', 't_old']
    err = weave.inline(code, args,
                       type_converters=weave.converters.blitz,
                       support_code=extra_code, compiler='gcc')
    return up
    

def bc_weave(u, bc, x, y, t):
    nx, ny = len(x)-1, len(y)-1
    extra_code = f.C_code('_f', inline=True) + \
                 bc.C_code('_bc', inline=True) + \
                 I.C_code('_I', inline=True)
    code = """
// boundary values of u
double xv, yv; int i, j;
i = 0; xv = x(i);
for (j=0; j<=ny; j++) { up(i,j) = _bc(xv, y(j), t); }
j = 0; yv = y(j);
for (i=0; i<=nx; i++) { up(i,j) = _bc(x(i), yv, t); }
i = nx; xv = x(i);
for (j=0; j<=ny; j++) { up(i,j) = _bc(xv, y(j), t); }
j = ny; yv = y(j);
for (i=0; i<=nx; i++) { up(i,j) = _bc(x(i), yv, t); }
"""
    args = ['up', 'nx', 'ny', 'x', 'y', 't']
    err = weave.inline(code, args,
                       type_converters=weave.converters.blitz,
                       support_code=extra_code, compiler='gcc')
    return up


def ic_scalar(u, um, I, f, bc, x, y, t, Cx2, Cy2, dt):
    nx, ny = len(x)-1, len(y)-1
    dt2 = dt*dt
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
    um = bc_scalar(um, bc, x, y, t+dt)

    return u, um
    
def scheme_scalar(up, u, um, f, x, y, t, Cx2, Cy2, dt2, t_old):
    nx, ny = len(x)-1, len(y)-1
    for i in iseq(start=1, stop=nx-1):
        for j in iseq(start=1, stop=ny-1):
            up[i,j] = - um[i,j] + 2*u[i,j] + \
                      Cx2*(u[i-1,j] - 2*u[i,j] + u[i+1,j]) + \
                      Cy2*(u[i,j-1] - 2*u[i,j] + u[i,j+1]) + \
                      dt2*f(x[i], y[j], t_old)
    return up

def bc_scalar(up, bc, x, y, t):
    nx, ny = len(x)-1, len(y)-1
    i = 0
    for j in iseq(0,ny): up[i,j] = bc(x[i], y[j], t)
    j = 0
    for i in iseq(0,nx): up[i,j] = bc(x[i], y[j], t)
    i = nx
    for j in iseq(0,ny): up[i,j] = bc(x[i], y[j], t)
    j = ny
    for i in iseq(0,nx): up[i,j] = bc(x[i], y[j], t)
    return up


def ic_vec(u, um, I, f, bc, x, y, xv, yv, t, Cx2, Cy2, dt):
    nx, ny = len(x)-1, len(y)-1
    dt2 = dt*dt
    print u.shape, um.shape
    # vectorized version:
    u[:,:] = I(xv,yv)  # works for scalar I too...
    um[1:nx,1:ny] = u[1:nx,1:ny] + \
      0.5*Cx2*(u[0:nx-1,1:ny] - 2*u[1:nx,1:ny] + u[2:nx+1,1:ny]) + \
      0.5*Cy2*(u[1:nx,0:ny-1] - 2*u[1:nx,1:ny] + u[1:nx,2:ny+1]) + \
      dt2*f(xv[1:nx,:], yv[:,1:ny], 0.0)
    # scalar f should be ok in all vectorized expressions
    
    # boundary values (evaluate at t=dt since du/dt=0 at t=0):
    um = bc_vec(um, bc, x, y, t+dt)
    return u, um
    

def scheme_vec(up, u, um, f, xv, yv, t, Cx2, Cy2, dt2, t_old):
    nx, ny = xv.shape[0]-1, yv.shape[1]-1
    up[1:nx,1:ny] = - um[1:nx,1:ny] + 2*u[1:nx,1:ny] + \
                    Cx2*(u[0:nx-1,1:ny] - 2*u[1:nx,1:ny] + u[2:nx+1,1:ny]) + \
                    Cy2*(u[1:nx,0:ny-1] - 2*u[1:nx,1:ny] + u[1:nx,2:ny+1]) + \
                    dt2*f(xv[1:nx,:], yv[:,1:ny], t_old)
    return up

def bc_vec(up, bc, x, y, t):
    nx, ny = len(x)-1, len(y)-1
    i = 0;  up[i,:] = bc(x[i], y, t)
    j = 0;  up[:,j] = bc(x, y[j], t)
    i = nx; up[i,:] = bc(x[i], y, t)
    j = ny; up[:,j] = bc(x, y[j], t)
    return up


    
def solver(I, f, c, bc, Lx, Ly, nx, ny, dt, tstop,
           user_action=None, 
           implementation={'ic': 'vec',  # or 'scalar' or 'weave'
                           'inner': 'vec',
                           'bc': 'vec',
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
    Two values are legal: 'scalar' or 'vec'.
    'scalar' means straight loops over grid points, while
    'vec' means special NumPy vectorized operations.
    If a key in the implementation dictionary is missing, it
    defaults in this function to 'scalar' (the safest strategy).
    Note that if 'vec' is specified, the functions I, f,
    and bc must work in vectorized mode. It is always recommended
    to first run the 'scalar' mode and then compare 'vec'
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

    if 'weave' in implementation.itervalues() or \
       'f77' in implementation.itervalues():
        # we avoid callback to Python and require f, bc, and I to be
        # string formulas:
        print f, bc, I
        if not isinstance(f,  StringFunction) or \
           not isinstance(bc, StringFunction) or \
           not isinstance(I,  StringFunction):
            raise TypeError, \
                  'with Weave or F77, f, bc, and I must be StringFunction'

    if 'f77' in implementation.itervalues():
        make_f77(f, bc, I) # build F77 module
        import f77
        # unified names with py versions:
        ic_f77 = f77.ic_f77
        bc_f77 = f77.bc_f77
        scheme_f77 = f77.scheme_f77
        
        # turn arrays to column major storage after the init. cond.

    # set initial condition:
    t0 = time.clock()
    t = 0.0
    print '***', implementation['ic']
    func = 'ic_'+implementation['ic']
    if func == 'ic_vec':
        u, um = eval(func)(u, um, I, f, bc, x, y, xv, yv, t, Cx2, Cy2, dt)
    elif func == 'ic_f77':
        u, um = eval(func)(u, um, x, y, Cx2, Cy2, dt)
    else:
        u, um = eval(func)(u, um, I, f, bc, x, y, t, Cx2, Cy2, dt)
    t_ic = time.clock() - t0
    

    if implementation['inner'] == 'f77':
        # turn input arrays to Fortran storage for all arrays
        # that are input arrays in loop subroutine
        # (actually not necessary as up, u, and um are all fed
        # through the f77.loop routine and brought to column
        # major storage in turn - recall um=u, u=up, up=um)
        if implementation.get('storage', 'f77') == 'f77':
            up = asarray(up, order='Fortran')
            u  = asarray(u,  order='Fortran')
            um = asarray(um, order='Fortran')

    if user_action is not None:
        user_action(u, xv, yv, t)  # allow user to plot etc.

    t_inner = 0                    # CPU time inner loops
    t_bc = 0                       # CPU time boundary update
    
    while t <= tstop:
        t_old = t;  t += dt
        if verbose:
            print 'solving (%s version) at t=%g' % \
                  (implementation['inner'], t)

        t0 = time.clock()
        # update all inner points:
        func = 'scheme_'+implementation['inner']
        if func == 'scheme_vec':
            up = eval(func)(up, u, um, f, xv, yv, t, Cx2, Cy2, dt2, t_old)
        elif func == 'scheme_f77':
            up = eval(func)(up, u, um, x, y, t, Cx2, Cy2, dt2, t_old)
        elif func == 'scheme_weave':
            up = eval(func)(up, u, um, f, bc, x, y, t, Cx2, Cy2, dt2, t_old)
        else:
            up = eval(func)(up, u, um, f, x, y, t, Cx2, Cy2, dt2, t_old)

        #id_u = id(u); id_um = id(um)
        #up,u,um = f77.loop(up, u, um, f_array, Cx2, Cy2, dt2)
        #print 'u changed:', id_u!=id(u),
        #print 'um changed:', id_um!=id(um),
        t_inner += time.clock() - t0

        t0 = time.clock()
        # insert boundary conditions:
        func = 'bc_'+implementation['bc']
        if func == 'bc_f77':
            up = eval(func)(up, x, y, t)
        else:
            up = eval(func)(up, bc, x, y, t)
        t_bc += time.clock() - t0
        
        if user_action is not None:
            user_action(up, xv, yv, t)
        # update data structures for next step:
        um, u, up = u, up, um

    # dt might be computed in this function
    return dt, t_ic, t_inner, t_bc


def verify_implementations(I, f, c, bc, Lx, Ly, nx, ny, tstop):
    """
    Run various modes (scalar, vec, f77, etc.)
    and compare results for verification.
    """

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
    versions = ('weave', 'scalar', 'vec', 'f77')
    implementation = {}
    for version in versions:
        for key in 'ic', 'inner', 'bc':
            implementation[key] = version
        action.init(version)
        print version
        solver(I, f, c, bc, Lx, Ly, nx, ny, 0, tstop,
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
    """
    Verify that scalar, vectorized, F77, weave implementations
    give the same answer in a test problem.
    """
    Lx = 10;  Ly = 10;  c = 1.0

    def I(x, y):
        return exp(-pow(x-Lx/2.0,2)/2.0 -pow(y-Ly/2.0,2)/2.0)
    def f(x, y, t):
        return sin(2*x) + y
    def bc(x, y, t):
        return sin(t)

    # use string formulas instead so also weave can be tested:
    # (need to transfer globals() so that vectorized versions work)
    I = StringFunction('exp(-pow(x-Lx/2.0,2)/2.0 - pow(y-Ly/2.0,2)/2.0)',
                       independent_variables=('x', 'y'),
                       Lx=Lx, Ly=Ly, globals=globals())
    f = StringFunction('sin(2*x) + y',
                       independent_variables=('x', 'y', 't'),
                       globals=globals())
    bc = StringFunction('sin(t)',
                        independent_variables=('x', 'y', 't'),
                        globals=globals())

    #nx = 15;  ny = 10; tstop = 2
    nx = 4;  ny = 3; tstop = 16
    verify_implementations(I, f, c, bc, Lx, Ly, nx, ny, tstop)
    

def test_plot1(plot=1, version='scalar'):
    """
    Initial Gaussian bell in the middle of the domain.
    plot: 0 = no plot; 1 = on the screen, 2 = hardcopy too
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

    I2 = StringFunction('exp(-(x-Lx/2.0)**2/2.0 -(y-Ly/2.0)**2/2.0)',
                        independent_variables=('x', 'y'),
                        Lx=Lx, Ly=Ly, globals=globals())
    f = StringFunction('0.0', independent_variables=('x', 'y', 't'),
                       globals=globals())
    bc = StringFunction('0.0', independent_variables=('x', 'y', 't'),
                        globals=globals())
    if plot:
        g = Gnuplot.Gnuplot(persist=1)
        g('set parametric')
        g('set data style lines')
        g('set hidden')
        g('set contour base')
        g('set zrange [-0.7:0.7]') # nice plot...
        
    def action(u, xv, yv, t):
        #print 'action, t=',t,'\nu=',u, '\nx=',x, '\ny=', y
        if plot:
            data = Gnuplot.GridData(u, xv[:,0], yv[0,:], binary=0)
            g.splot(data)
            g('set title "t=%g"' % t)
            if plot == 2:
                g.hardcopy(filename='tmp_%020f.ps' % t, enhanced=1, mode='eps',
                           color=0, fontname='Times-Roman', fontsize=14)
                time.sleep(1)
            time.sleep(0.2) # pause between frames

    implementation = {'ic': version, 'inner': version, 'bc': version}
    nx = 40; ny = 40; tstop = 20 # tstop = 700
    print 'test_plot1:', f, bc, I2
    dt, t_ic, t_inner, t_bc = \
    solver(I2, f, c, bc, Lx, Ly, nx, ny, 0, tstop,
           user_action=action, implementation=implementation)
    print 'time ic: %s, time scheme: %s, time bc: %s' % (t_ic, t_inner, t_bc)
    time.sleep(3)


def visualize(I, f, c, bc, Lx, Ly, nx, ny, dt, tstop,
              zmin, zmax, plot=1, version='scalar',
              implementation=\
              {'ic': 'scalar', 'inner': 'scalar', 'bc': 'scalar'}):
    """
    A general wrapper of function solver with additional visualization
    of the solution.
    """
    class Visualizer:
        """Action function used by solver."""
        def __init__(self, plot=0):
            self.plot = plot
            if self.plot:
                self.g = Gnuplot.Gnuplot(persist=1)
                self.g('set parametric')
                self.g('set data style lines')
                self.g('set hidden')
                self.g('set contour base')
                self.g('set zrange [%g:%g]' % (zmin,zmax))

        def __call__(self, u, xv, yv, t):
            if self.plot:
                data = Gnuplot.GridData(u, xv[:,0], yv[0,:], binary=0)
                self.g.splot(data)
                self.g('set title "t=%g"' % t)
            if self.plot == 2:
                self.g.hardcopy(filename='tmp_%020f.ps' % t,
                                enhanced=1, mode='eps', fontsize=14,
                                color=0, fontname='Times-Roman')
                time.sleep(0.8)   # pause to finish plot

    viz = Visualizer(plot)
    dt, cpu_ic, cpu_inner, cpu_bc = \
        solver(I, f, c, bc, Lx, Ly, nx, ny, dt, tstop,
               user_action=viz, implementation=implementation)
    time.sleep(3)


def test_bell(version='scalar'):
    Lx = 10
    Ly = 10
    c = 1.0

    def I(x, y):
        return exp(-(x-Lx/2.0)**2/2.0 -(y-Ly/2.0)**2/2.0)
    def f(x, y, t):
        return 0.0
    def bc(x, y, t):
        return 0.0

    Lx = 10
    Ly = 10
    c = 1.0
    nx = 40; ny = 40; tstop = 700
    dt = 0
    implementation = {'ic': version, 'inner': version, 'bc': version}
    visualize(I, f, c, bc, Lx, Ly, nx, ny, dt, tstop,
              -1.2, 1.2, plot=1, version=version,
              implementation=implementation)
    
    
def test_error1(version='scalar'):
    """Analytical solution (bc=u). Compute errors."""
    Lx = 10
    Ly = 10
    c = 1.0

    def exact(x, y, t):
        kx = pi/Lx; ky = pi/Ly; omega = sqrt(kx*kx + ky*ky)
        return cos(omega*t)*sin(kx*x)*sin(ky*y)

    def I1(x, y):
        return exact(x, y, 0)

    def bc(x, y, t):
        return exact(x, y, t)

    def f(x, y, t):
        if isinstance(x, ndarray) and isinstance(y, ndarray):
            return zeros((x.shape[0], y.shape[1]))
        else:
            return 0.0
    
    error = []
    def action(u, xv, yv, t):
        e = exact(xv, yv, t) - u
        error.append((t, sqrt(innerproduct(e.flat,e.flat))))

    t0 = time.clock()
    implementation = {'ic': version, 'inner': version, 'bc': version}
    nx = 10; ny = 4; tstop = 20
    solver(I1, f, c, bc, Lx, Ly, nx, ny, 0, tstop,
           user_action=action, implementation=implementation)
    for t, e in error:
        print 't=%10.2E  error=%10.2E' % (t, e)

def test_plug(version='scalar'):
    """
    Display movie of plug-shaped wave. Not a 1D exact solution because of
    the y=const u=0 B.C. 
    """
    Lx = 10
    Ly = 10
    c = 1.0

    def bc(x, y, t):
        if isinstance(x, ndarray):
            return zeros(size(x))
        elif isinstance(y, ndarray):
            return zeros(size(y))
        else:  # scalar case
            return 0.0

    def I(x, y):
        """Plug shaped initial condition."""
        plug_length = 2
        if isinstance(x, ndarray) and isinstance(y, ndarray):
            # vectorized version
            u = zeros((x.shape[0], y.shape[1]))
            #u1 = less(x, Lx/2.0 - 0.1)
            #u1 += greater(x, Lx/2.0 + 0.1)
            #put(u, u1, 2.0) does not work, u1 should be 2D array
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
        if isinstance(x, ndarray) and isinstance(y, ndarray):
            return zeros((x.shape[0], y.shape[1]))
        else:
            return 0.0
    
    import time

    g = Gnuplot.Gnuplot(persist=1)
    g('set yrange [-1.2:1.2]')
    def action(u, xv, yv, t):
        xcoor = xv[:,0]
        ucurve = u[:,2]
        g.plot(Gnuplot.Data(xcoor, ucurve, with_='lines'))

    implementation = {'ic': version, 'inner': version, 'bc': version}
    nx = 40; ny = 30; tstop = 20
    dt = Lx/float(nx)/c
    dt = 0
    visualize(I, f, c, bc, Lx, Ly, nx, ny, dt, tstop,
              -1.2, 1.2, plot=1, implementation=implementation)
    time.sleep(3)
    
def benchmark(nx, tstop):
    """Initial Gaussian bell in the middle of the domain."""
    Lx = 10
    Ly = 10
    c = 1.0
    ny = nx

    # our use of weave requires string formulas:
    Is = StringFunction('exp(-pow(x-Lx/2.0,2)/2.0 -pow(y-Ly/2.0,2)/2.0)',
                        independent_variables=('x','y'),
                        Lx=Lx, Ly=Ly, globals=globals())
    fs = StringFunction('0.0', independent_variables=('x', 'y', 't'),
                        globals=globals())
    BCs = StringFunction('0.0', independent_variables=('x', 'y', 't'),
                         globals=globals())

    def action(u, xv, yv, t):
        #print t
        pass

    implementation = {}
    cpu = []
    for ic in 'f77', 'vec', 'scalar', 'weave':
        for bc in 'f77', 'vec', 'scalar', 'weave':
            for inner in 'f77', 'vec', 'scalar', 'weave':
                implementation['ic'] = ic
                implementation['inner'] = inner
                implementation['bc'] = bc
                # optimize StringFunction functions for the non-weave case:
                # implementation:
                if 'weave' in (ic, bc, inner) or 'f77' in (ic, bc, inner):
                    I = Is;  f = fs;  BC = BCs
                else:
                    I = Is.__call__;  f = fs.__call__;  BC = BCs.__call__

                t0 = time.clock()
                dt, cpu_ic, cpu_inner, cpu_bc = \
                    solver(I, f, c, BC, Lx, Ly, nx, ny, 0, tstop,
                           user_action=None,
                           implementation=implementation,
                           verbose=False)
                t1 = time.clock()
                cpu_total = cpu_ic + cpu_inner + cpu_bc
                overhead = (t1-t0)-cpu_total
                cpu.append([implementation.copy(), cpu_total,
                            cpu_ic, cpu_inner, cpu_bc, overhead])
                print t1-t0, implementation, 'overhead:', overhead
    # normalize CPU-times:
    cpu_min = min([abs(c) for i, c, c1, c2, c3, c4 in cpu])
    print '\n\nMinimum CPU time:', cpu_min
    print 'no of time steps:', int(tstop/dt)
    print 'interior/boundary ratio:', int(nx*ny*1.0/max(nx,ny))
    for impl, cpu, cpu_ic, cpu_inner, cpu_bc, overhead in cpu:
        # normalized-CPU  ic  inner  bc  overhead
        print "%8.2f" % (cpu/cpu_min),
        print "%-10s %8.2f; " % (impl['ic'], cpu_ic),
        print "%-10s %8.2f; " % (impl['inner'], cpu_inner),
        print "%-10s %8.2f; " % (impl['bc'], cpu_bc),
        print "%d%%" % (overhead/cpu*100)

    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print """Usage %s test_plot1 1 "'vec'" """ % \
              sys.argv[0]
        sys.exit(0)
    cmd = '%s(%s)' % (sys.argv[1], ', '.join(sys.argv[2:]))
    print cmd
    exec(cmd)
