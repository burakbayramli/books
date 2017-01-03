#!/usr/bin/env python

"""
Put in a NumPyDB, no plotmtv!
"""

from scitools.numpyutils import *
import wave2D  # F77 functions
solve_at_this_time_step = wave2D.solveatthistimestep
to_fortran = wave2D.as_column_major_storage
# make sys.path so we can find Grid2D.py:
sys.path.insert(0, os.path.join(os.environ['scripting'],
                                'src','py','examples'))
from Grid2D import Grid2D

class Field2D:
    def __init__(self, grid, name, values=None):
        self.init(grid, name, values)

    def init(self, grid, name, values=None):
        self.grid = grid
        g = self.grid # short form used below
        # make grid aware of its fields:
        try:
            g.fields[name] = self
        except:
            # self.grid.fields is not initialized; bring into play:
            g.fields = {name: self}

        self.name = name
        if values is None:
            # create array of scalar field grid point values:
            self.values = zeros((len(g.xcoor),len(g.ycoor)))
        else:
            self.values = values  # field data are provided
            # should check compatibility of the values array...

    def __del__(self):
        # disconnect from grid:
        del self.grid.fields[self.name]

    def __getitem__(self, i):
        if isinstance(i, slice):
            return self.values[i]  # forward slice to NumPy

    def __setitem__(self, i, value):
        if isinstance(i, slice):
            self.values[i] = value # forward slice to NumPy

    def min(self):
        return min(ravel(self.values))

    def max(self):
        return max(ravel(self.values))

        
def dump_plotmtv(field, filename, zmin=None, zmax=None):
    pass

class Wave2D:
    def __init__(self):
        pass

    def init(self, m, n):
        dx = 10.0/(m-1);  dy = 10.0/(n-1)
        self.g = Grid2D(xmin=0, xmax=10, dx=dx,
                        ymin=0, ymax=10, dy=dy)
        #self.up = zeros((m,n))
        #self.u  = self.up.copy()
        #self.um = self.up.copy()

        self.up = Field2D(self.g, 'up')
        self.u  = Field2D(self.g, 'u')
        self.um = Field2D(self.g, 'um')
        self.lambda_ = Field2D(self.g, 'lambda')

    def timeloop0(self, nsteps, IC, lambda_):
        # set initial condition:
        self.u .values = self.g(IC)
        # rough approximation to du/dt=0:
        self.um.values = self.u.values.copy()
        # get right storage for efficient F77 communication:
        self.u .values = to_fortran(self.u .values)
        self.um.values = to_fortran(self.um.values)
        self.up.values = to_fortran(self.up.values)
        self.lambda_.values = to_fortran(self.g(lambda_))

        wave2D.dump(self.u.values, 0, 0.0)
        # optimal time step:
        self.dt = 1.0/self.lambda_.max()/\
                  sqrt(1.0/self.g.dx**2+1.0/self.g.dy**2)
        t = 0
        for timelevel in range(nsteps):
            t += self.dt
            print 'solve at time', t
            self.up.values = solve_at_this_time_step(\
                self.up.values, self.u.values, self.um.values,
                self.lambda_.values, self.dt)
            wave2D.dump(self.u.values, timelevel, t)
            self.um.values = self.u .values.copy()
            self.u .values = self.up.values.copy()

    def timeloop(self, nsteps, IC, lambda_):
        # set initial condition:
        self.u .values = self.g(IC)
        # rough approximation to du/dt=0:
        self.um.values = self.u.values.copy()
        # get right storage for efficient F77 communication:
        self.u .values = to_fortran(self.u .values)
        self.um.values = to_fortran(self.um.values)
        self.up.values = to_fortran(self.up.values)
        self.lambda_.values = to_fortran(self.g(lambda_))

        # short forms for reading or in-place modifications:
        u = self.u.values
        um = self.um.values
        up = self.up.values
        l = self.lambda_.values
        dx = self.g.dx; dy = self.g.dy

        wave2D.dump(u, 0, 0.0)

        # optimal time step:
        self.dt = 1.0/self.lambda_.max()/sqrt(1.0/dx**2+1.0/dy**2)
        t = 0
        for timelevel in range(nsteps):
            t += self.dt
            print 'solve at time', t
            # u = ... is okay, think about it; u is forgotten
            # when we leave this routine, but solve_at_this_time_step
            # performs in-place modifications of the array u pointed
            # to and points to (the same array)
            # NO: u=... is not okay, need u[:,:] as in-place mod.
            # understand why!
            u[:,:] = solve_at_this_time_step(up, u, um, l, self.dt)
            wave2D.dump(u, timelevel, t)
            um[:,:] = u  # in-place update
            u [:,:] = up

# plotmtv -3d -colorps -o tmp.ps -noxplot -nodate tmp_00015.mtv

if __name__ == '__main__':
    w = Wave2D()
    import glob
    for file in glob.glob('*.mtv'): os.remove(file)
    w.init(30, 30)
    w.timeloop(100,
               # note that IC and lambda_ must vectorize:
               lambda x,y: exp(-x**2-y**2),
               lambda x,y: zeros((x.shape[0], y.shape[1])) + 1.0)
    #os.system('plotmtv -3d *.mtv')
    os.system('mtv2mpeg -p "-colorps -3d -nodate" *.mtv')
        
