#!/usr/bin/env python

from numpy import *
from numpy import f2py  # not part of import *
from scitools.StringFunction import StringFunction
import time, sys, os
# make sys.path so we can find Grid2D.py:
sys.path.insert(0, os.path.join(os.environ['scripting'],
                                'src','py','examples'))
from Grid2D import Grid2D
try:
    import ext_gridloop
except ImportError:
    print 'You must first build the ext_gridloop module'
    sys.exit(1)


class Grid2Deff(Grid2D):

    def ext_gridloop1(self, f):
        """Compute a[i,j] = f(xi,yj) in an external routine."""
        # a is made here, sent to the routine, and then returned
        a = zeros((self.xcoor.size, self.ycoor.size))
        # C/C++ or Fortran module?
        if ext_gridloop.__doc__ is not None:
            if 'f2py' in ext_gridloop.__doc__:
                # Fortran extension module
                a = asarray(a, order='Fortran')
        # could store a as self.a to avoid making Fortran
        # arrays in repeated calls

        ext_gridloop.gridloop1(a, self.xcoor, self.ycoor, f)
        return a

    def ext_gridloop2(self, f):
        """Compute a[i,j] = f(xi,yj) in an external routine."""
        # a is made in the external routine
        a = ext_gridloop.gridloop2(self.xcoor, self.ycoor, f)
        return a

    def ext_gridloop_exceptions(self, f):
        """Test error handling in the extension module."""
        try: #1
            ext_gridloop.gridloop1((1,2), self.xcoor, self.ycoor[1:], f)
        except: 
            print sys.exc_type, sys.exc_value
        try: #2
            ext_gridloop.gridloop1(self.xcoor, self.xcoor, self.ycoor[1:], f)
        except:
            print sys.exc_type, sys.exc_value
        try: #3
            ext_gridloop.gridloop2(self.xcoor, self.ycoor, 'abc')
        except:
            print sys.exc_type, sys.exc_value
        try: #4
            ext_gridloop.gridloop2(array(self.xcoor,Complex64),
                                   self.ycoor, 'abc')
        except:
            print sys.exc_type, sys.exc_value
        try: #5
            ext_gridloop.gridloop2(array([[0,0],[1,2]]), self.ycoor, 'abc')
        except:
            print sys.exc_type, sys.exc_value

    # NOTE: the three next functions are only available in the
    # Fortran 77 extension module:
    
    def ext_gridloop_vec1(self, f):
        """As ext_gridloop2, but vectorized callback."""
        a = zeros((self.xcoor.size, self.ycoor.size))
        a = ext_gridloop.gridloop_vec1(a, self.xcoor, self.ycoor, f)
        return a

    def ext_gridloop_vec2(self, f):
        """As ext_gridloop_vec1, but callback to func. w/grid arg."""
        a = zeros((self.xcoor.size, self.ycoor.size))
        a = ext_gridloop.gridloop_vec2(a, f, func1_extra_args=(self,))
        return a

    def myfuncf3(self, a):
        a[:,:] = myfunc(self.xcoorv, self.ycoorv)  # in-place mod.

    def ext_gridloop_vec3(self, f):
        """As ext_gridloop_vec2, but callback to class method."""
        a = zeros((self.xcoor.size, self.ycoor.size))
        a = ext_gridloop.gridloop_vec2(a, f)
        return a

    def ext_gridloop2_str(self, f77_name):
        """
        Call an interface to ext_gridloop.gridloop2, which avoids
        callbacks to Python and calls the f77_name F77 function
        instead.
        """
        a = ext_gridloop.gridloop2_str(self.xcoor, self.ycoor,
                                       f77_name)
        return a

    def ext_gridloop_noalloc(self, f77_name, a):
        """
        As ext_gridloop2_str, but a is intent(in,out), i.e., there is
        no allocation of a in the wrapper code. If the function
        is called a large number of times (as in our efficiency
        tests), intent(in,out) increases the performance.
        """
        a = ext_gridloop.gridloop_noalloc(a, self.xcoor, self.ycoor,
                                          f77_name)
        return a
    

    def ext_gridloop2_fcb(self):
        """As ext_gridloop2, but compiled Fortran callback func."""
        import callback
        a = callback.gridloop2_fcb(self.xcoor, self.ycoor)
        return a

    def ext_gridloop2_fcb_compile(self, fstr):
        if not isinstance(fstr, str):
            raise TypeError, \
            'fstr must be string expression, not %s', type(fstr)
        
        # generate Fortran source
        source = """
      real*8 function fcb(x, y)
      real*8 x, y
      fcb = %s
      return
      end

      subroutine gridloop2_fcb(a, xcoor, ycoor, nx, ny)
      integer nx, ny
      real*8 a(nx,ny), xcoor(nx), ycoor(ny)
Cf2py intent(out) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor
Cf2py depend(nx,ny) a
      real*8 fcb
      external fcb

      call gridloop2(a, xcoor, ycoor, nx, ny, fcb)
      return
      end
""" % fstr
        # compile callback code and link with ext_gridloop.so:
        f2py_args = "--fcompiler=Gnu --build-dir=tmp2"\
                    " -DF2PY_REPORT_ON_ARRAY_COPY=1 "\
                    " ./ext_gridloop.so"
        r = f2py.compile(source, modulename='callback',
                         extra_args=f2py_args, verbose=True,
                         source_fn='_cb.f')
        if r:
            print 'unsuccessful compilation'; sys.exit(1)
        import callback  # see if we can import successfully


    def ext_gridloop2_fcb_ptr(self):
        """As ext_gridloop2, but compiled Fortran callback func."""
        from callback import fcb
        a = ext_gridloop.gridloop2(self.xcoor, self.ycoor,
                                   fcb._cpointer)
        return a

    def ext_gridloop2_fcb_ptr_compile(self, fstr):
        if not isinstance(fstr, StringFunction):
            raise TypeError, \
            'fstr must be StringFunction, not %s', type(fstr)

        source = fstr.F77_code('fcb')
        f2py_args = "--fcompiler=Gnu --build-dir=tmp2"
        r = f2py.compile(source, modulename='callback',
                         extra_args=f2py_args, verbose=True,
                         source_fn='_cb.f')
        if r:
            print 'unsuccessful compilation'; sys.exit(1)
        import callback  # see if we can import successfully


    def ext_gridloop2_compile(self, fstr):
        if not isinstance(fstr, str):
            raise TypeError, \
            'fstr must be string expression, not', type(fstr)
        
        # generate Fortran source for gridloop2:
        source = """
      subroutine gridloop2(a, xcoor, ycoor, nx, ny)
      integer nx, ny
      real*8 a(nx,ny), xcoor(nx), ycoor(ny)
Cf2py intent(out) a
Cf2py depend(nx,ny) a

      integer i,j
      real*8 x, y
      do j = 1,ny
         y = ycoor(j)
         do i = 1,nx
            x = xcoor(i)
            a(i,j) = %s
         end do
      end do
      return
      end
""" % fstr        
        f2py_args = "--fcompiler=Gnu --build-dir tmp1"\
                    " -DF2PY_REPORT_ON_ARRAY_COPY=1"
        r = f2py.compile(source, modulename='ext_gridloop2',
                         extra_args=f2py_args, verbose=True,
                         source_fn='_cb.f')
        if r:
            print 'unsuccessful compilation'; sys.exit(1)
        import ext_gridloop2  # see if we can import successfully

    def ext_gridloop2_v2(self):
        """
        As ext_gridloop2, but the Fortran gridloop2 function was
        generated and compiled in Python (in ext_gridloop2_compile).
        """
        import ext_gridloop2
        return ext_gridloop2.gridloop2(self.xcoor, self.ycoor)

    def ext_gridloop2_weave(self, fstr):
        """Migrate loop to C++ with aid of Weave."""
        try:
            from scipy import weave
        except ImportError:
            print 'Could not import weave.\nContinue...'
            return
        
        if not isinstance(fstr, str):
            raise TypeError, \
            'fstr must be string expression, not', type(fstr)

        # the callback function is now coded in C++
        # (fstr must be valid C++ code):
        extra_code = r"""
double cppcb(double x, double y) {
  return %s;
}
""" % fstr
        # the loop in C++ (with Blitz++ array syntax):
        code = r"""
int i,j;
for (i=0; i<nx; i++) {
  for (j=0; j<ny; j++) {
    a(i,j) = cppcb(xcoor(i), ycoor(j));
  }
}
"""
        nx = self.nx;  ny = self.ny
        a = zeros((nx, ny))
        xcoor = self.xcoor;  ycoor = self.ycoor
        err = weave.inline(code, ['a', 'nx', 'ny', 'xcoor', 'ycoor'],
              type_converters=weave.converters.blitz,
              support_code=extra_code, compiler='gcc')
        # a is filled
        return a
    

    def ext_gridloop1_instant(self, fstr):
        if not isinstance(fstr, str):
            raise TypeError, \
            'fstr must be string expression, not', type(fstr)
        
        # generate C source for gridloop1:
        # (no call to C function f(x,y), fstr is inserted in the loop)
        source = """
void gridloop1(double *a, int nx, int ny,
               double *xcoor, double *ycoor)
{
# define index(a, i, j)  a[i*ny + j]
  int i, j;  double x, y;
  for (i=0; i<nx; i++) {
    for (j=0; j<ny; j++) {
      x = xcoor[i];  y = ycoor[i];
      index(a, i, j) = %s
    }
  }
}
""" % fstr

        try:
            from instant import inline_with_numpy
            a = zeros((self.nx, self.ny))
            arrays = [['nx', 'ny', 'a'],
                      ['nx', 'xcoor'],
                      ['ny', 'ycoor']]
            self.gridloop1_instant = \
                 inline_with_numpy(source, arrays=arrays)
        except:
            self.gridloop1_instant = None


    def dump(self, a):
        """Nice printout of a 2D array a."""
        for i in xrange(a.shape[0]):
            for j in xrange(a.shape[1]):
                print 'value at (%g,%g)  \t = a[%d,%d] = %g' % \
                      (self.xcoor[i], self.ycoor[j], i, j, a[i,j])

    def gridloop_psyco_init(self, method):
        """Try to accelerate Grid2D.gridloop with psyco."""
        # define method self.gridloop_psyco:
        try:
            import psyco
            self.gridloop_psyco = psyco.proxy(method)
        except ImportError:
            self.gridloop_psyco = method
    
def f1(x,y):
    print 'x+2*y =',x+2*y
    return x+2*y

def verify1():
    """Basic test of the extension module."""
    g = Grid2Deff(dx=0.5, dy=1)
    f_exact = g(f1)  # NumPy computation

    expression1 = StringFunction('x + 2*y',
                                 independent_variables=('x','y'),
                                 globals=globals())

    f = g.ext_gridloop1(f1)
    print 'f computed by external gridloop1 function and f1:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'

    f = g.ext_gridloop2(f1)
    print 'f computed by external gridloop2 function and f1:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'

    f = g.ext_gridloop1(expression1)
    print 'f computed by external gridloop1 function and StringFunction:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'

    f = g.ext_gridloop2(expression1)
    print 'f computed by external gridloop2 function and StringFunction:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'

    fast_func = expression1.__call__
    f = g.ext_gridloop2(fast_func)
    print 'f computed by external gridloop2 function and StringFunction.__call__:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'

    f = g(expression1)
    print 'f computed by __call__ and StringFunction:\n', f
    if allclose(f, f_exact, atol=1.0E-10, rtol=1.0E-12):
        print 'f is correct'

    # check printing:
    print 'array seen from Python:'
    g.dump(f)
    if 'dump' in dir(ext_gridloop):
        print 'array seen from Fortran (transposed, but right values):'
        ext_gridloop.dump(f, g.xcoor, g.ycoor)

def myfunc(x, y):
    return sin(x*y) + 8*x

def myfuncf1(a, xcoor, ycoor, nx, ny):
    """Vectorized function to be called from extension module."""
    #print 'myfuncf1; type of args:',type(a),type(xcoor),type(nx)
    x = xcoor[:,newaxis]
    y = ycoor[newaxis,:]
    a[:,:] = myfunc(x, y)  # in-place modification of a
    print 'myfuncf1, a=',a

def myfuncf2(a, g):
    """Vectorized function to be called from extension module."""
    #print 'myfuncf2; type of args:',type(a),type(g)
    a[:,:] = myfunc(g.xcoorv, g.ycoorv)  # in-place modification of a


def verify2(n=3):
    """
    Test of some methods in class Grid2Deff that call up
    some F77 routines for improving the efficiency of callbacks
    to Python.
    """

    if not 'gridloop_vec2' in dir(ext_gridloop):
        raise ImportError, 'verify2 works only for F77 module'
    dx = 1.0/n
    g = Grid2Deff(dx=dx, dy=dx)

    from StringIO import StringIO
    from scitools.numpyutils import arr
    a_exact = arr(file_=StringIO("""
    
       0.          0.          0.          0.        
       2.66666667  2.7775493   2.88706441  2.99386136
       5.33333333  5.55373108  5.7632897   5.95170314
       8.          8.3271947   8.6183698   8.84147098"""))

    def _check():
        if not allclose(a, a_exact):
            print 'ERROR, a is wrong, correct a reads\n', a_exact
        else:
            print 'correct array'

    a = g.ext_gridloop_vec1(myfuncf1)
    print "g.ext_gridloop_vec1(myfuncf1): a=\n",a
    _check()
    a = g.ext_gridloop_vec2(myfuncf2)
    print "g.ext_gridloop_vec2(myfuncf2): a=\n",a
    _check()
    # need f2py version > 2.42 (callback to class method):
    a = g.ext_gridloop_vec3(g.myfuncf3)
    print "g.ext_gridloop_vec3(g.myfuncf3): a=\n",a
    _check()
    a = g.ext_gridloop2_str('myfunc')
    print "g.ext_gridloop_str('myfunc'): a=\n",a
    _check()
    a = g.ext_gridloop_noalloc('myfunc', a)
    print "g.ext_gridloop_str_noalloc('myfunc'): a=\n",a
    _check()

    fstr = 'sin(x*y) + 8*x'
    g.ext_gridloop2_fcb_compile(fstr)
    a = g.ext_gridloop2_fcb()
    print "g.gridloop2_fcb: a=\n",a
    _check()
    import callback
    print 'contents of callback module:', dir(callback)

    fstr = StringFunction('sin(x*y) + 8*x')
    g.ext_gridloop2_fcb_ptr_compile(fstr)
    a = g.ext_gridloop2_fcb_ptr()
    print "g.gridloop2_fcb_ptr: a=\n",a
    _check()
    import callback
    print 'fcb callback module:', dir(callback), dir(callback.fcb)


    g.ext_gridloop2_compile(fstr)
    a = g.ext_gridloop2_v2()
    print "g.gridloop2_v2: a=\n",a
    _check()
    a = g.ext_gridloop2_weave(fstr)
    print "g.gridloop2_weave: a=\n",a
    _check()
    g.gridloop_psyco_init(g.gridloop)
    a = g.gridloop_psyco(fstr)
    print "g.gridloop_psyco(str): a=\n",a
    _check()
    a = g.gridloop_psyco(myfunc)
    print "g.gridloop_psyco(func): a=\n",a
    _check()
    g.ext_gridloop1_instant(fstr)
    g.gridloop1_instant(a, g.nx, g.ny, g.xcoor, g.ycoor)
    print "g.gridloop1_instant: a=\n", a


def timing2(n=2000, best_time=1.0):
    """Time different implementations of the extension module."""
    print 'Grid2Deff.timing2: reference CPU time = %g' % best_time
    
    dx = 1.0/n
    g = Grid2Deff(dx=dx, dy=dx)
    # here we use straight NumPy sin in a scalar context:
    def myfunc1(x, y):
        return sin(x*y) + 8*x

    def myfunc2(x, y):
        return math.sin(x*y) + 8*x

    expression1 = StringFunction('sin(x*y) + 8*x',
                                 independent_variables=('x','y'),
                                 globals=globals())
    expression1_f = expression1.__call__  # for efficiency and F77 callback

    expression2 = StringFunction('math.sin(x*y) + 8*x',
                                 independent_variables=('x','y'),
                                 globals=globals())
    expression2_f = expression2.__call__  # for efficiency and F77 callback
    
    from scitools.misc import timer
    from scitools.EfficiencyTable import EfficiencyTable
    e = EfficiencyTable('Grid2Deff tests, %dx%d grid' % (n,n), best_time)

    t0a = timer(g.gridloop, (myfunc1,), repetitions=1)
    e.add('g.gridloop, myfunc1', t0a)
    t0b = timer(g.gridloop, (myfunc2,), repetitions=1)
    e.add('g.gridloop, myfunc2', t0b)
    t0c = timer(g.__call__, (myfunc1,), repetitions=1)
    e.add('g.__call__, myfunc1', t0c)
    t0d = timer(g.__call__, (expression1_f,), repetitions=1)
    e.add('g.__call__, expression1_f', t0d)
    t0e = timer(g.gridloop_itemset, (myfunc2,), repetitions=1)
    e.add('g.gridloop_itemset, myfunc2', t0e)

    t1a = timer(g.ext_gridloop1, (myfunc1,), repetitions=1)
    e.add('g.ext_gridloop1, myfunc1', t1a)
    t1b = timer(g.ext_gridloop1, (myfunc2,), repetitions=1)
    e.add('g.ext_gridloop1, myfunc2', t1b)
    t2a = timer(g.ext_gridloop2, (myfunc1,), repetitions=1)
    e.add('g.ext_gridloop2, myfunc1', t2a)
    t2b = timer(g.ext_gridloop2, (myfunc2,), repetitions=1)
    e.add('g.ext_gridloop2, myfunc2', t2b)
    t3a = timer(g.ext_gridloop2, (expression1_f,), repetitions=1)
    e.add('g.ext_gridloop2, expression1_f', t3a)
    t3b = timer(g.ext_gridloop2, (expression2_f,), repetitions=1)
    e.add('g.ext_gridloop2, expression2_f', t3b)
    nrep = 20

    # try the improved functions (works only for the F77 module):
    if 'gridloop_vec2' in dir(ext_gridloop):
        t4 = timer(g.ext_gridloop_vec2, (myfuncf2,), repetitions=nrep)
        e.add('g.ext_gridloop_vec2, myfuncf2', t4)

    if 'gridloop2_str' in dir(ext_gridloop):        
        t5 = timer(g.ext_gridloop2_str, ('myfunc',), repetitions=nrep)
        e.add('g.ext_gridloop2_str, myfunc', t5)

        # try the version without allocation (first, make an a array):
        a = g.ext_gridloop2(myfunc1)  # a has now Fortran storage
        t5b = timer(g.ext_gridloop_noalloc,
                    ('myfunc', a), repetitions=nrep)
        e.add('g.ext_gridloop_noalloc, myfunc', t5b)
        
        # try 'inline' F77 compiled callback too:
        # (give F77 source for core of callback function as argument)

        g.ext_gridloop2_fcb_compile(str(expression1))
        t6 = timer(g.ext_gridloop2_fcb, (), repetitions=nrep)
        e.add('g.ext_gridloop2_fcb(%s)' % repr(str(expression1)), t6)

        g.ext_gridloop2_fcb_ptr_compile(expression1)
        t6b = timer(g.ext_gridloop2_fcb_ptr, (), repetitions=nrep)
        e.add('g.ext_gridloop2_fcb_ptr(%s)' % repr(expression1), t6b)

        g.ext_gridloop2_compile(str(expression1))
        t7 = timer(g.ext_gridloop2_v2, (), repetitions=nrep)
        e.add('g.ext_gridloop2_v2(%s)' % repr(str(expression1)), t7)

    # weave version:
    t8 = timer(g.ext_gridloop2_weave, (str(expression1),), repetitions=nrep)
    e.add('g.ext_gridloop2_weave(%s)' % repr(str(expression1)), t8)

    # psyco:
    g.gridloop_psyco_init(g.gridloop)
    if g.gridloop_psyco != g.gridloop:  # has psyco
        t9a = timer(g.gridloop_psyco, (myfunc2,), repetitions=1)
        e.add('g.gridloop_psyco, myfunc2', t9a)
        t9b = timer(g.gridloop_psyco, (expression2_f,), repetitions=1)
        e.add('g.gridloop_psyco, expression2_f', t9b)

    g.gridloop_psyco_init(g.gridloop_itemset)
    if g.gridloop_psyco != g.gridloop_itemset:  # has psyco
        t9a = timer(g.gridloop_psyco, (myfunc2,), repetitions=1)
        e.add('g.gridloop_psyco (itemset), myfunc2', t9a)
        t9b = timer(g.gridloop_psyco, (expression2_f,), repetitions=1)
        e.add('g.gridloop_psyco (itemset), expression2_f', t9b)

    # instant:
    g.ext_gridloop1_instant(str(expression1))
    if g.gridloop1_instant is not None:
        a = zeros((self.nx, self.ny))
        t10 = timer(g.gridloop1_instant,
                    (a, self.nx, g.ny, g.xcoor, g.ycoor),
                    repetitions=nrep)
        e.add('g.gridloop1_instant', t10)

    print '\n\n\n\nrun from directory', os.getcwd()
    print e
    #print 'Experiments in table:', e.experiments

def exceptions1():
    """Test exceptions raised by the extension module."""
    g = Grid2Deff(dx=0.5, dy=1)
    def myfunc(x, y):
        return sin(x*y) + 8*x
    g.ext_gridloop_exceptions(myfunc)

def run():
    # provide function to call (verify1, timing2, exceptions1, etc.)
    # as command-line argument
    try:
        func = sys.argv[1]
    except:
        # basic test if no command-line argument
        func = 'verify1'
    if func == 'timing2':
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
        exec 'timing2(%d, %g)' % (n, best_time)
    else:
        exec func + '()'
    
if __name__ == '__main__':
    # lots of experiments:
    # Grid2Deff.py timing2 1100 0.13
    # 1100 is grid size, 0.13 is reference time
    run()
