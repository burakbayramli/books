#!/usr/bin/env python
# as Grid2Deff.py but explicit conversion between NumPy objects
# and MyArray objects are performed (via SWIG pointer exchange).

from numpy import *
import time, sys, Gnuplot, os
import ext_gridloop

# load ../../Grid2Deff.py:
sys.path.insert(0, os.path.join(os.pardir, os.pardir))
from Grid2Deff import f1, Grid2Deff
               
class Grid2Deff2(Grid2Deff):
    def __init__(self,
                 xmin=0, xmax=1, dx=0.5,
                 ymin=0, ymax=1, dy=0.5):
        Grid2Deff.__init__(self, xmin, xmax, dx, ymin, ymax, dy)
        self.c = ext_gridloop.Convert_MyArray()
        
    def ext_gridloop1(self, f):
        """Compute a[i,j] = f(xi,yj) in an external routine."""
        a = zeros((self.nx, self.ny))
        # C/C++ or Fortran module?
        if ext_gridloop.__doc__ is not None:
            if 'f2py' in ext_gridloop.__doc__:
                a = asarray(a, order='Fortran')
        a_p = self.c.py2my(a)
        x_p = self.c.py2my(self.xcoor)
        y_p = self.c.py2my(self.ycoor)
        f_p = self.c.set_pyfunc(f)
        ext_gridloop.gridloop1(a_p, x_p, y_p, f_p)
        return a

    def ext_gridloop2(self, f):
        """Compute a[i,j] = f(xi,yj) in an external routine."""
        x_p = self.c.py2my(self.xcoor)
        y_p = self.c.py2my(self.ycoor)
        f_p = self.c.set_pyfunc(f)
        a_p = ext_gridloop.gridloop2(x_p, y_p, f_p)
        a = self.c.my2py(a_p)
        return a

    def ext_gridloop_exceptions(self, f):
        """Test error handling in the extension module."""
        x_p = self.c.py2my(self.xcoor)
        y_p1 = self.c.py2my(self.ycoor[1:])
        f_p = self.c.set_pyfunc(f)
        try: #1
            ext_gridloop.gridloop1((1,2), x_p, y_p1, f_p)
        except: 
            print sys.exc_type, sys.exc_value
        try: #2
            ext_gridloop.gridloop1(x_p, x_p, y_p1, f_p)
        except:
            print sys.exc_type, sys.exc_value
        try: #3
            ext_gridloop.gridloop2(x_p, y_p, 'abc')
        except:
            print sys.exc_type, sys.exc_value

def verify1():
    g = Grid2Deff2(dx=0.5, dy=1)
    f_exact = g(f1)
    f = g.ext_gridloop1(f1)
    print 'f computed by external gridloop1 function:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'
    f = g.ext_gridloop2(f1)
    print 'f computed by external gridloop2 function:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'

    # check printing:
    print 'array seen from Python:'
    g.dump(f)
    if 'dump' in dir(ext_gridloop):
        print 'array seen from Fortran (transposed, but right values):'
        ext_gridloop.dump(f, g.xcoor, g.ycoor)
    
def timing3(n=2000, best_time=1.0):
    print 'Grid2Deff2.timing2: reference CPU time = %g' % best_time
    
    dx = 1.0/n
    g = Grid2Deff2(dx=dx, dy=dx)

    # here we use straight NumPy sin in a scalar context:
    def myfunc1(x, y):
        return sin(x*y) + 8*x
    def myfunc2(x, y):
        return math.sin(x*y) + 8*x

    from scitools.misc import timer
    from scitools.EfficiencyTable import EfficiencyTable
    e = EfficiencyTable('Grid2Deff2 tests', best_time)
    print 'basic NumPy module:', basic_NumPy
    if basic_NumPy != 'Numeric':
        print 'only Numeric is meaningful for the C++ extension module'
        return
    t1a = timer(g.ext_gridloop1, (myfunc1,), repetitions=1)
    e.add('g.ext_gridloop1, myfunc1', t1a)
    t1b = timer(g.ext_gridloop1, (myfunc2,), repetitions=1)
    e.add('g.ext_gridloop1, myfunc2', t1b)
    t2a = timer(g.ext_gridloop2, (myfunc1,), repetitions=1)
    e.add('g.ext_gridloop2, myfunc1', t2a)
    t2b = timer(g.ext_gridloop2, (myfunc2,), repetitions=1)
    e.add('g.ext_gridloop2, myfunc2', t2b)
    print e

def run():
    # provide function to call (verify1, timing2, exceptions1, etc.)
    # as command-line argument
    try:
        func = sys.argv[1]
    except:
        # basic test if no command-line argument
        func = 'verify1'
    if func == 'timing3':
        # in case of timing, specify grid size as 2nd argument:
        try:
            n = int(sys.argv[2])
        except:
            n = 1100
        # specify reference executing time as 3rd argument:
        try:
            best_time = float(sys.argv[3])
        except:
            best_time = 1.0
        exec 'timing3(%d, %g)' % (n, best_time)
    else:
        exec func + '()'

def exceptions1():
    print 'Grid2Deff2.exceptions1:'
    g = Grid2Deff2(dx=0.5, dy=1)
    def myfunc(x, y):
        return sin(x*y) + 8*x
    g.ext_gridloop_exceptions(myfunc)

    
if __name__ == '__main__':
    run()
