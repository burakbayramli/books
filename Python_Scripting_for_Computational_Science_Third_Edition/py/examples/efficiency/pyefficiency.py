#!/usr/bin/env python
"""
Measure the efficiency of different Python constructions.
"""

from scitools.misc import timer, timer_system
import sys, math, random, timeit, os
gmathsin = math.sin  # global name
from numpy import *

# easiest to import weave into global namespace:
try:
    from scipy import weave   # SciPy 0.4
except ImportError:
    try:
        import weave
    except:
        print 'Could not import weave - this may cause aborts'


from scitools.EfficiencyTable import EfficiencyTable

from scitools.misc import hardware_info    
            
def range_tests(n):
    """
    How much does it cost to run an empty loop?
    Different loop constructions: range, xrange, iseq (from numpytools).
    """

    e = EfficiencyTable('Empty loops, loop length = %d' % n)
    t1 = timeit.Timer('for i in range(n): pass',
                      setup='n=%d' % n).timeit(5)
    e.add('for i in range(n): pass', t1)
    t2 = timeit.Timer('for i in xrange(n): pass',
                      setup='n=%d' % n).timeit(5)
    e.add('for i in xrange(n): pass', t2)
    t3 = timeit.Timer('for i in iseq(stop=n-1): pass',
                      setup='from scitools.numpytools import iseq;' +
                      'n=%d' % n).timeit(5)
    e.add('for i in iseq(stop=n-1): pass', t3)
    print e

    e = EfficiencyTable('List comprehension, map, etc., loop length = %d' % n)
    t4 = timeit.Timer('[i for i in xrange(n)]',
                      setup='n=%d' % n).timeit(5)
    e.add('[i for i in xrange(n)]', t4)
    t5 = timeit.Timer('map(lambda i: i, xrange(n))',
                      setup='n=%d' % n).timeit(5)
    e.add('map(lambda i: i, xrange(n))', t5)
    print e

def factorials(n):
    """
    Evaluate the efficiency of different factorial methods from
    numpyutils.factorial.

    n is the number of repetitions of N! (N=80)
    """
    N = 80
    e = EfficiencyTable('Factorial of N=%d, repetition=%d' % (N, n))
    methods = 'plain recursive', \
              'lambda recursive', \
              'lambda functional', \
              'lambda list comprehension', \
              'reduce', \
              'scipy'

    for method in methods:
        t1 = timeit.Timer('factorial(%d, method="%s")' % (N, method),
             setup='from scitools.numpyutils import factorial').timeit(n)
        e.add('%d! %s' % (N, method), t1)
    print e

def allocate_tests(n):
    """
    Compare list constructions: append vs [0]*n vs NumPy's zeros,
    and simple number to put it (i+2) vs a number that requires
    significant computations (random.gauss).
    """

    def list_append1(n):
        r = []
        for i in xrange(n):
            r.append(i+2)
        return r

    def list_append2(n):
        return [i+2 for i in xrange(n)]

    def list_chunk1(n):
        r = [0.0]*n
        for i in xrange(n):
            r[i] = i+2
        return r

    def list_append3(n):
        import random
        r = []
        for i in xrange(n):
            r.append(random.gauss(0,1))
        return r

    def list_chunk2(n):
        import random
        r = [0.0]*n
        for i in xrange(n):
            r[i] = random.gauss(0,1)
        return r

    def list_append4(n):
        import random
        g = random.gauss
        r = []
        for i in xrange(n):
            r.append(g(0,1))
        return r

    def list_append5(n):
        import random
        return [random.gauss(0,1) for i in xrange(n)]

    def list_chunk3(n):
        import random
        g = random.gauss
        r = [0.0]*n
        for i in xrange(n):
            r[i] = g(0,1)
        return r


    e1 = EfficiencyTable('allocate n numbers (i+2) in an array/list: n = %d' % n)
    e2 = EfficiencyTable('allocate n random numbers in an array/list: n = %d' % n)
    rep = 3
    e1.add('r=[]; r.append(i+2)',
           timer(list_append1, (n,), repetitions=rep))
    e1.add('[i+2 for i in xrange(n)]',
           timer(list_append2, (n,), repetitions=rep))
    e1.add('r=[0]*n; r[i] = i+2', timer(list_chunk1,  (n,), repetitions=rep))

    e2.add('r=[]; r.append(random.gauss(0,1))',
           timer(list_append3, (n,), repetitions=rep))
    e2.add('r=[0]*n; r[i] = random.gauss(0,1)',
           timer(list_chunk2,  (n,), repetitions=rep))
    e2.add('r.append(g(0,1))  (g=random.gauss)',
           timer(list_append4, (n,), repetitions=rep))
    e2.add('[random.gauss(0,1) for i in xrange(n)]',
           timer(list_append5, (n,), repetitions=rep))
    e2.add('r=[0]*n; r[i] = g(0,1)  (g=random.gauss)',
           timer(list_chunk3,  (n,), repetitions=rep))

    e1.add('linspace(0, 1, n)',
           timeit.Timer('linspace(0, 1, %d)' % n,
                        setup='from numpy import linspace').timeit(rep)/float(rep))
    e1.add('zeros(n, float) (from numpy)',
           timeit.Timer('zeros(%d, float)' % n,
                        setup='from numpy import zeros').timeit(rep)/float(rep))
    e2.add('numpy.random.random(n)',
           timeit.Timer('random.random(%d)' % n,
                        setup='from numpy import random').timeit(rep)/float(rep))
    e2.add('RandomArray.random(n) (from Numeric)',
           timeit.Timer('RandomArray.random(%d)' % n,
                        setup='import RandomArray').timeit(rep)/float(rep))
    
    for module in 'numpy', 'Numeric', 'numarray':
        try:
            e1.add('arange(0, 1, 1./(n-1)) (from %s)' % module,
                   timeit.Timer('arange(0, 1, 1.0/(%d-1))' % n,
                   setup='from %s import arange' % module).timeit(rep)/float(rep))
            e1.add('zeros(n) (from %s)' % module,
                   timeit.Timer('zeros(%d)' % n,
                   setup='from %s import zeros' % module).timeit(rep)/float(rep))
            e1.add('fromfunction(lambda i: i*1.0/(n-1), (n,))  (from %s)' % module,
                   timeit.Timer('fromfunction(lambda i: i*1.0/(%d-1), (%d,))' % (n,n),
                   setup='from %s import fromfunction' % module).timeit(rep)/float(rep))
            e1.add('fromfunction(lambda i: i+2, (n,))  (from %s)' % module,
                   timeit.Timer('fromfunction(lambda i: i+2, (%d,))' % n,
                   setup='from %s import fromfunction' % module).timeit(rep)/float(rep))
        except ImportError:
            print 'Module %s was not installed - skipping it' % module

    print e1
    print e2

def type_tests(n, rep=5000):
    """Check isinstance vs type vs length tests."""

    e = EfficiencyTable('Checking type: isinstance, type, etc. (%d rep.)' % n)
    setup = """\
from numpy import ndarray, linspace, zeros
from types import FloatType
import operator
x = linspace(0, 1, %d)
""" % n
    code = ['isinstance(x, float)',
            'isinstance(x, ndarray)',
            'type(x) == type(zeros(2,float))',
            'type(x) == FloatType',
            'operator.isNumberType(x)',
            ]
    tests = [[timeit.Timer(c, setup=setup).timeit(rep), c] \
             for c in code]
    for t, c in tests:
        e.add(c, t)
    print e

def size_tests(n, rep=5000):
    """Check isinstance vs type vs length tests."""

    e = EfficiencyTable('Checking numpy size comparisons; x is %d-vector' % n)
    setup = """\
from numpy import linspace, shape, size
x = linspace(0, 1, %d)
""" % n
    code = ['len(x) == 1',
            'size(x) == 1',
            'x.shape == (1,)',
            'shape(x) == (1,)',
            ]
    tests = [[timeit.Timer(c, setup=setup).timeit(rep), c] \
             for c in code]
    for t, c in tests:
        e.add(c, t)
    print e

    
def call_tests(rep, x, y, z):
    """
    Measure the overhead of function calls:
    empty functions, functions with some computations,
    functions with new calls, functions with loops,
    functions in Fortran, functions with keyword arguments,
    overhead of scitools.wrap2callable wrappers, the cost of
    many (1,2,3,...) arguments, etc.
    """
    
    try:
        from call import loop, fempty, fwconsts, flops
        fempty.__name__ = 'empty_func in F77'
        fwconsts.__name__ = 'func_with_consts in F77'
    except ImportError:
        print 'Run f2py -c -m call call.f'
        sys.exit(1)

    def empty_func(x, y, z):
        pass

    def func_with_consts1(x, y, z):
        a = 0.3
        b = 1.2
        c = 1.22E+02
        q = a*x + b*y + c*z
        return q
    
    def func_with_consts2(x, y, z, a=0.3, b=1.2, c=1.22E+02):
        q = a*x + b*y + c*z
        return q

    def func_with_consts3(x, y, z):
        # hardcoded coefficients
        q = 0.3*x + 1.2*y + 1.22E+02*z
        return q

    def _help(x):
        return x+1
    
    def func_loop_with_call(n):
        r = 0.1
        for i in xrange(n):
            r = _help(r)

    def func_loop_with_inline(n):
        r = 0.1
        for i in xrange(n):
            r = r + 1

            
    # test NumPy vs math sin for scalar arguments:
    # see sin_comparison.py
    
    print '\n*** Testing function calls ***\n'
    e1 = EfficiencyTable('Calling a constant function f(x)=2.0')
    setup = """
from scitools.numpytools import wrap2callable
from math import sin
f = wrap2callable"""
    e1.add('f = wrap2callable(2.0);  f(0.9)  [const wrapper]',
           timeit.Timer('f(0.9)', setup=setup+'(2.0)').timeit(rep))
    e1.add('f = wrap2callable(lambda x: 2.0);  f(0.9)  [func wrapper]',
           timeit.Timer('f(0.9)', setup=setup+'(lambda x: 2.0)').timeit(rep))
    e1.add('f = wrap2callable("2.0");  f(0.9)  [StringFunction]',
           timeit.Timer('f(0.9)', setup=setup+'("2.0")').timeit(rep))
    e1.add('f = wrap2callable("2.0");  f = f.__call__; f(0.9) [StringFunction]',
           timeit.Timer('f(0.9)', setup=setup+'("2.0"); f = f.__call__').timeit(rep))
    fclass1 = """
class MyFunc:
    def __call__(self, x):
        return 2.0
f = MyFunc()
fm = f.__call__
"""
    e1.add('function object f; f(0.9)  [callable wrapper]',
           timeit.Timer('f(0.9)', setup=fclass1).timeit(rep))
    e1.add('function object f; fm = f.__call__; fm(0.9)  [callable wrapper]',
           timeit.Timer('fm(0.9)', setup=fclass1).timeit(rep))
    e1.add('def f(x): return 2.0; f(0.9)',
           timeit.Timer('f(0.9)', setup='def f(x): return 2.0').timeit(rep))

    # test alternative StringFunction (slower) implementations:
    for stf in range(1,6):
        setup = """\
from scitools.StringFunction import StringFunction_v%d
f = StringFunction_v%d('2.0')
""" % (stf, stf)
        e1.add('f = StringFunction_v' + str(stf) + '("2.0"); f(0.9)',
               timeit.Timer('f(0.9)', setup=setup).timeit(rep))
    print e1
           
    setup = """
from scitools.numpytools import wrap2callable
from math import sin
f = wrap2callable"""
    e2 = EfficiencyTable('Calling a constant function f(x,y,z)=2.0')
    e2.add('f = wrap2callable(2.0);  f(0.9, 0.1, 1)',
           timeit.Timer('f(0.9, 0.1, 1)', setup=setup+'(2.0)').timeit(rep))
           #, independent_variables=("x","y","z")
    e2.add('f = wrap2callable(lambda x,y,z: 2.0);  f(0.9, 0.1, 1)',
           timeit.Timer('f(0.9, 0.1, 1)', setup=setup+'(lambda x,y,z: 2.0)').timeit(rep))
    e2.add('f = wrap2callable("2.0");  f(0.9, 0.1, 1)',
           timeit.Timer('f(0.9, 0.1, 1)', setup=setup+'("2.0", independent_variables=("x1", "x2", "x3"))').timeit(rep))
    print e2

    e2 += e1  # add and compare with e1 experiments
    print e2
    

    e3 = EfficiencyTable('Calling f(x,y,z)=sin(x)*sin(y)*sin(z)')
    e3.add('f = wrap2callable(lambda x,y,z: ...)',
           timeit.Timer('f(1,1,1)',
           setup=setup+'(lambda x,y,z: sin(x)*sin(y)*sin(z))').timeit(rep))
    e3.add('f = wrap2callable("sin(x)*sin(y)*sin(z)");  f(1,1,1)',
           timeit.Timer('f(1,1,1)',
            setup=setup+'("sin(x)*sin(y)*sin(z)", ' + \
                        'independent_variables=("x","y","z"))').timeit(rep))
    fclass3 = """
from math import sin
class MyFunc:
    def __call__(self, x, y, z):
        return sin(x)*sin(y)*sin(z)
f = MyFunc()
"""
    e3.add('function object f; f(1,1,1)',
           timeit.Timer('f(1,1,1)', setup=fclass3).timeit(rep))
    e3.add('function object f; fm = f.__call__; fm(1,1,1)',
           timeit.Timer('fm(1,1,1)', setup=fclass3 + '\nfm = f.__call__\n').timeit(rep))
    print e3

    # F77 versions:

    # measure the costs in terms of flops:
    repfactor = 50  # need more repetitions here than in the above tests
    f77rep = 100*repfactor
    c = timer(flops, (f77rep*rep,), repetitions=1,
              comment='measuring flops cost in F77:')
    one_flop = c/float(8*f77rep*rep)
    print 'one flop in F77 is', one_flop, 'second'

    e4 = EfficiencyTable('Calling functions, Py vs F77')
    t1 = timer(empty_func, (x, y, z), repetitions=rep*repfactor)
    e4.add('Python empty_func', t1)
    t1 = timer(fempty, (x, y, z), repetitions=rep*repfactor)
    e4.add('Python fall to F77 fempty', t1)
    
    c = timer(loop, (f77rep*rep, 'fempty'), repetitions=1,
              comment='loop over fempty in  F77:')
    c1 = c/float(f77rep*rep)  # time for a single call in F77
    e4.add('F77 fempty', c1)
    print '\nOne function call in Python costs %.1f F77 flops' % (t1/one_flop)
    print 'One function call in F77 costs %.1f F77 flops' % (c1/one_flop)
          
    t = timer(fwconsts, (x, y, z), repetitions=rep*repfactor)
    e4.add('Py call to F77 fwconsts', t)    
    c = timer(loop, (f77rep*rep, 'fwconsts'), repetitions=1,
              comment='loop over fwconsts in F77:')
    c = c/float(f77rep*rep)
    e4.add('F77 fwconsts', c)
    print e4
    
    e5 = EfficiencyTable('Calling functions, func_with_consts* family')
    t = timer(func_with_consts1, (x, y, z), repetitions=rep*repfactor,
              comment='constants in statements:')
    e5.add('func_with_consts1 (constants in statements)', t)
    t = timer(func_with_consts2, (x, y, z), repetitions=rep*repfactor,
              comment='constants as default kwargs:')
    e5.add('func_with_consts2 (constants as default kwargs)', t)
    t = timer(func_with_consts3, (x, y, z), repetitions=rep*repfactor,
              comment='constants hardcoded:')
    e5.add('func_with_consts3 (constants hardcoded)', t)
    print e5

    e6 = EfficiencyTable('Calling functions, loop with call vs inline func')
    t = timer(func_loop_with_call, (rep,), repetitions=1*repfactor,
              comment='loop with function call:')
    e5.add('func_loop_with_call (loop w/call)', t)

    t = timer(func_loop_with_inline, (rep,), repetitions=1*repfactor,
              comment='loop with inline expression:')
    e5.add('func_loop_with_inline (loop w/inline func)', t)
    print e5
    
    # many arguments:
    call_nargs_test(rep)

    """
    Python 2.3 on laptop IBM X30:
    a function call costs about 3 flops
    calling empty_func and func_with_consts gives compatible results
    a sin(x) call is as fast as 3 flops
    """

def generate_func_with_many_args(n):
    code = """
def func_with_many_args(%s):
    return True
""" % ', '.join(['a%d' % i for i in range(1,n+1)])
    return code

def call_nargs_test(rep):
    for n in range(1,101,20):
        code = generate_func_with_many_args(n)
        exec code in globals(), globals()
        print timer(func_with_many_args, tuple(range(n)), repetitions=rep,
                comment='empty func with %d arguments:' % n)

    
def flop_tests(n):
    """
    Test the efficiency of floating point operations: one multiplication
    per pass in a loop 'for i in xrange(n)'. Python and Fortran.
    """
    try:
        from call import loop, fempty, fwconsts, flops
        fempty.__name__ = 'empty_func in F77'
        fwconsts.__name__ = 'func_with_consts in F77'
    except ImportError:
        print 'Run f2py -c -m call call.f'
        sys.exit(1)


    def empty_loop(n):
        for i in xrange(n):
            pass
        
    def flops_py(n):
        b = 1.0000001
        a = 1.1
        for i in xrange(n):
            a = a*b
        return a

    print '\n\n*** Multiplication test ***\n'
    t1 = timeit.Timer('a*b*c', setup='a=1.01; b=0.98; c=0.99').timeit(n)
    print n, 'multiplications in a loop'
    t2 = timer(flops, (n,), repetitions=100, comment='F77:')
    # result in F77 is multiplication _with_ loop
    best = min(t1, t2)
    print 'multiplication: python=%.2f F77=%.2f' % (t1/best, t2/best)


def triple_numpy_loops(n, rep):
    
    e = EfficiencyTable('Triple nested loops over 3D array %dx%dx%d=%d' \
                        % (n,n,n,n*n*n))
    setup = """
from numpy import ndenumerate, zeros
n = %d
a = zeros((n,n,n))
""" % n
    code = """
for i in xrange(a.shape[0]):
    for j in xrange(a.shape[1]):
        for k in xrange(a.shape[2]):
            a[i,j,k] = -1
"""
    t = timeit.Timer(code, setup=setup).timeit(rep)
    e.add('3 xrange loops, a[i,j,k]', t)

    code = """
for i in xrange(a.shape[0]):
    for j in xrange(a.shape[1]):
        for k in xrange(a.shape[2]):
            a[i][j][k] = -1
"""
    t = timeit.Timer(code, setup=setup).timeit(rep)
    e.add('3 xrange loops, a[i][j][k]', t)

    code = """
for index, v in ndenumerate(a):
    i,j,k = index
    a[i,j,k] = -1
"""
    t = timeit.Timer(code, setup=setup).timeit(rep)
    e.add('ndenumerate, a[i,j,k]', t)
    print e

    
def matrixfill_tests(languages, n):
    """
    Test the performance of filling matrix entries in Python,
    Fortran, and C++.
    """

    """
    1000x1000 matrix:
    
reference CPU time: 0.0327

Fill F77 matrix in Fortran loops.                                          1.00
Fill C++ matrix in C++ loops.                                              1.21
Fill NumPy matrix in Fortran loops.                                        3.95
Fill F77 matrix in Fortran loops; sin/exp formula.                         12.42
Fill NumPy matrix in Fortran loops; sin/exp formula.                       15.62
Fill C++ matrix in C++ loops; sin/exp formula.                             15.92
Fill NumPy matrix in Python loop; a[i][j] indexing.                        70.03
Avoid proxy class, call Matrix_set directly.                               107.74
Avoid proxy class, call _matrix_cpp.Matrix_set directly.                   117.40
Fill F77 matrix in a Python loop with F77 set calls.                       155.57
Fill NumPy matrix in a Python loop with F77 set calls.                     202.39
Fill C++ matrix in a Python loop with m.set indexing.                      203.52
Fill NumPy matrix in Python loop; sin/exp formula; a[i][j]                 249.54
Avoid proxy class, call Matrix_set directly; sin/exp formula.              272.48
Avoid proxy class, call _matrix_cpp.Matrix_set directly; sin/exp formula.  285.75
Fill F77 matrix in a Python loop with F77 set calls; sin/exp.              322.69
Fill NumPy matrix in a Python loop with F77 set calls; sin/exp.            376.91
Fill C++ matrix in a Python loop with m.set indexing; sin/exp.             390.86
Fill NumPy matrix in Python loop.                                          445.87
Fill NumPy matrix in Python loop; sin/exp formula.                         662.08

numarray:
reference CPU time: 0.0394

Fill C++ matrix in C++ loops.                                              1.00
Fill C++ matrix in C++ loops; sin/exp formula.                             13.39
Avoid proxy class, call Matrix_set directly.                               90.08
Avoid proxy class, call _matrix_cpp.Matrix_set directly.                   97.11
Fill NumPy matrix in Python loop; a[i][j] indexing.                        110.91
Fill C++ matrix in a Python loop with m.set indexing.                      170.15
Fill NumPy matrix in Python loop.                                          205.84
Avoid proxy class, call Matrix_set directly; sin/exp formula.              225.43
Avoid proxy class, call _matrix_cpp.Matrix_set directly; sin/exp formula.  237.61
Fill NumPy matrix in Python loop; sin/exp formula; a[i][j]                 294.92
Fill C++ matrix in a Python loop with m.set indexing; sin/exp.             327.77
Fill NumPy matrix in Python loop; sin/exp formula.                         389.09

numpy:
reference CPU time: 0.0333

Fill C++ matrix in C++ loops.                                              1.00
Fill C++ matrix in C++ loops; sin/exp formula.                             15.54
Fill NumPy matrix in Python loop; a[i][j] indexing.                        81.08
Avoid proxy class, call Matrix_set directly.                               106.91
Avoid proxy class, call _matrix_cpp.Matrix_set directly.                   115.20
Fill C++ matrix in a Python loop with m.set indexing.                      202.31
Avoid proxy class, call Matrix_set directly; sin/exp formula.              265.26
Fill NumPy matrix in Python loop; sin/exp formula; a[i][j]                 268.77
Avoid proxy class, call _matrix_cpp.Matrix_set directly; sin/exp formula.  279.34
Fill C++ matrix in a Python loop with m.set indexing; sin/exp.             382.10
Fill NumPy matrix in Python loop.                                          516.22
Fill NumPy matrix in Python loop; sin/exp formula.                         727.33
    """
    if isinstance(languages, str):
        languages = [languages]

    from math import sin, exp
    if 'F77' in languages:
        try:
            from matrix_f77 import makematrix, set, tonumpy, adump, \
                 fill1, fill2, lfill1, lfill2, set_a, matrixdata
            import matrix_f77
        except ImportError:
            print 'Run f2py -c m matrix matrix_f77.f (or ./make_f77.sh)'
            sys.exit(1)
        makematrix(n, n)  # make matrix in F77
    if 'C++' in languages:
        try:
            from matrix_cpp import Matrix
            from _matrix_cpp import Matrix_set
            import _matrix_cpp  # for efficiency comparison
        except ImportError:
            print 'run make_cpp.sh'
            sys.exit(1)
        m = Matrix(n)

    a = zeros((n, n))

    def setmatrix1_py():
        """Fill NumPy matrix in Python loop."""
        for i in xrange(n):
            for j in xrange(n):
                a[i, j] = i*j-2
        return a

    def setmatrix2_py():
        """Fill NumPy matrix in Python loop; sin/exp formula."""
        for i in xrange(n):
            x = i*0.1
            for j in xrange(n):
                y = j*0.1
                a[i, j] = sin(x)*sin(y)*exp(-x*y)
        return a

    def setmatrix1b_py():
        """Fill NumPy matrix in Python loop; a[i][j] indexing."""
        for i in xrange(n):
            for j in xrange(n):
                a[i][j] = i*j-2
        return a

    def setmatrix2b_py():
        """Fill NumPy matrix in Python loop; sin/exp formula; a[i][j]"""
        for i in xrange(n):
            x = i*0.1
            for j in xrange(n):
                y = j*0.1
                a[i][j] = sin(x)*sin(y)*exp(-x*y)
        return a


    #======== F77 functions =========

    def setmatrix1_f_index():
        """Fill F77 matrix in a Python loop with F77 set calls."""
        for i in xrange(n):
            for j in xrange(n):
                set(i, j, i*j-2)
        r = tonumpy(n, n)
        # could perhaps tune the interface file such that tonumpy
        # doesn't need arguments
        return r

    def setmatrix2_f_index():
        """Fill F77 matrix in a Python loop with F77 set calls; sin/exp."""
        for i in xrange(n):
            x = 0.1*i
            for j in xrange(n):
                y = 0.1*j
                set(i, j, sin(x)*sin(y)*exp(-x*y))
        r = tonumpy(n, n)
        return r

    def setmatrix1_f_index_a():
        """Fill NumPy matrix in a Python loop with F77 set calls."""
        global af
        for i in xrange(n):
            for j in xrange(n):
                af = set_a(af, i, j, i*j-2)
                # note that the first time, a is copied and transposed
                # by the wrapper code, but this is done only once
        return af

    def setmatrix2_f_index_a():
        """Fill NumPy matrix in a Python loop with F77 set calls; sin/exp."""
        global af
        for i in xrange(n):
            x = 0.1*i
            for j in xrange(n):
                y = 0.1*j
                af = set_a(af, i, j, sin(x)*sin(y)*exp(-x*y))
                # note that the first time, a is copied and transposed
                # by the wrapper code, but this is done only once
        return af

    def setmatrix1_f_loop1():
        """Fill F77 matrix in Fortran loops."""
        fill1()  # all loops in F77
        #r = tonumpy(n, n)  # we don't do this in C++ version

    def setmatrix1_f_loop2():
        """Fill NumPy matrix in Fortran loops."""
        # use af to avoid copying to column major storage when
        # this function is called a large number of times
        global af  
        r = lfill1(af)  # all loops in F77, fill NumPy array
        return r

    def setmatrix2_f_loop1():
        """Fill F77 matrix in Fortran loops; sin/exp formula."""
        fill2()  # all loops in F77
        #r = tonumpy(n, n)  # we don't do this in C++ version

    def setmatrix2_f_loop2():
        """Fill NumPy matrix in Fortran loops; sin/exp formula."""
        global af
        r = lfill2(af)  # all loops in F77, fill NumPy array
        return r

    #======== C++ functions =========

    def setmatrix1_c_index1():
        """Fill C++ matrix in a Python loop with m.set indexing."""
        for i in xrange(n):
            for j in xrange(n):
                m.set(i, j, i*j-2)
        # could perhaps tune the interface file such that tonumpy
        # doesn't need arguments
        return m

    def setmatrix2_c_index1():
        """Fill C++ matrix in a Python loop with m.set indexing; sin/exp."""
        for i in xrange(n):
            x = 0.1*i
            for j in xrange(n):
                y = 0.1*j
                m.set(i, j, sin(x)*sin(y)*exp(-x*y))
        return m

    def setmatrix1_c_index3():
        """Avoid proxy class, call Matrix_set directly."""
        for i in xrange(n):
            for j in xrange(n):
                Matrix_set(m, i, j, i*j-2)
        # could perhaps tune the interface file such that tonumpy
        # doesn't need arguments
        return m

    def setmatrix2_c_index3():
        """Avoid proxy class, call Matrix_set directly; sin/exp formula."""
        for i in xrange(n):
            x = 0.1*i
            for j in xrange(n):
                y = 0.1*j
                Matrix_set(m, i, j, sin(x)*sin(y)*exp(-x*y))
        return m

    def setmatrix1_c_index4():
        """Avoid proxy class, call _matrix_cpp.Matrix_set directly."""
        for i in xrange(n):
            for j in xrange(n):
                _matrix_cpp.Matrix_set(m, i, j, i*j-2)
        # could perhaps tune the interface file such that tonumpy
        # doesn't need arguments
        return m

    def setmatrix2_c_index4():
        """Avoid proxy class, call _matrix_cpp.Matrix_set directly; sin/exp formula."""
        for i in xrange(n):
            x = 0.1*i
            for j in xrange(n):
                y = 0.1*j
                _matrix_cpp.Matrix_set(m, i, j, sin(x)*sin(y)*exp(-x*y))
        return m


    def setmatrix1_c_loop1():
        """Fill C++ matrix in C++ loops."""
        m.fill1()  # all loops in C++
        return m

    def setmatrix2_c_loop1():
        """Fill C++ matrix in C++ loops; sin/exp formula."""
        m.fill2()  # all loops in C++
        return m


    #======== end of C++ functions =========

    e = EfficiencyTable('Matrix indexing; size %dx%d' % (n,n))

    for f in setmatrix1_py, setmatrix2_py, setmatrix1b_py, setmatrix2b_py:
        t = timer(f, (), repetitions=1)
        e.add(f.__doc__, t)
        
    if 'F77' in languages:
        try:  # avoid crash for numarray objects:
            import matrix_f77
            if n != matrix_f77.matrixdata.n or \
               n != matrix_f77.matrixdata.m:
                raise ValueError, 'F77 matrix is %dx%d '\
                      'while Python matrix is %dx%d' % \
                      (matrix_f77.matrixdata.m, matrix_f77.matrixdata.n, n, n)
            
            global af
            af = asarray(a, order='Fortran')
            for f in setmatrix1_f_index, setmatrix2_f_index, \
                setmatrix1_f_index_a, setmatrix2_f_index_a:
                t = timer(f, (), repetitions=10)
                e.add(f.__doc__, t)
            for f in setmatrix1_f_loop1, setmatrix1_f_loop2, \
                setmatrix2_f_loop1, setmatrix2_f_loop2:
                t = timer(f, (), repetitions=100)
                e.add(f.__doc__, t)
        except Exception, exception:
            print 'F77 module did not work properly\n', exception

    if 'C++' in languages:
        for f in setmatrix1_c_index1, setmatrix2_c_index1, \
            setmatrix1_c_index3, setmatrix2_c_index3, \
            setmatrix1_c_index4, setmatrix2_c_index4:
            t = timer(f, (), repetitions=10)
            e.add(f.__doc__, t)
        for f in setmatrix1_c_loop1, setmatrix2_c_loop1:
            t = timer(f, (), repetitions=100)
            e.add(f.__doc__, t)
    print e


def matvec_prod_tests(n):
    """
    Test the efficiency of various implementations of a
    matrix-vector product: plain Python loops, NumPy dot,
    Weave, Psyco, Fortran, functional programming, etc.
    """

    def prod1(m, v):
        nrows, ncolumns = m.shape
        res = zeros(nrows)
        for i in xrange(nrows):
            for j in xrange(ncolumns):
                res[i] += m[i,j]*v[j]
        return res
    
    def prod2(m, v):
        nrows = m.shape[0]
        res = zeros(nrows)
        for i in range(nrows):
            res[i] = reduce(lambda x,y: x+y, map(lambda x,y: x*y, m[i,:], v))
        return res

    def prod3(m, v):
        nrows = m.shape[0]
        index = xrange(nrows)
        return array(map(lambda i: reduce(lambda x,y: x+y, m[i,:]*v),index))

    try:
        from call import prod4, prod5
        prod4.__name__ = 'prod4'
        prod5.__name__ = 'prod5'
    except ImportError:
        print 'Run f2py -c -m call call.f'
        sys.exit(1)

    def prod7(m, v):
        nrows, ncolumns = m.shape
        res = zeros(nrows)
        code = r"""
for (int i=0; i<nrows; i++) {
    for (int j=0; j<ncolumns; j++) {
        res(i) += m(i, j)*v(j);
    }
}
        """
        try:
            from scipy import weave
            err = weave.inline(code, ['nrows', 'ncolumns', 'res', 'm', 'v'],
                  type_converters=weave.converters.blitz, compiler='gcc')
            return res
        except ImportError, e:
            print e
            return None

    m = ones((n,n))
    v = ones(n)
    import Numeric
    m_Numeric = Numeric.ones((n,n))
    v_Numeric = Numeric.ones(n)
    
    e = EfficiencyTable('Matrix-vector product, n=%d' % n)

    t1 = timer(prod1, args=(m,v), repetitions=5, comment='plain loops')
    e.add('plain loops', t1)
    t2 = timer(prod2, args=(m,v), repetitions=5, comment='reduce/map')
    e.add('reduce/map prod2', t2)
    t3 = timer(prod3, args=(m,v), repetitions=5, comment='improved reduce/map')
    e.add('improved reduce/map prod3', t3)

    t1 = timer(prod1, args=(m_Numeric,v_Numeric), repetitions=5, comment='plain loops')
    e.add('plain loops, prod1, Numeric', t1)
    t2 = timer(prod2, args=(m_Numeric,v_Numeric), repetitions=5, comment='reduce/map')
    e.add('reduce/map, prod2, Numeric', t2)
    t3 = timer(prod3, args=(m_Numeric,v_Numeric), repetitions=5, comment='improved reduce/map')
    e.add('improved reduce/map, prod3, Numeric', t3)

    t7 = timer(dot, args=(m,v), repetitions=50,
               comment='NumPy dot')
    e.add('numpy.dot', t7)
    t7 = timer(Numeric.dot, args=(m_Numeric,v_Numeric), repetitions=50,
               comment='NumPy dot')
    e.add('Numeric.dot', t7)

    try:
        import psyco
        prod6 = psyco.proxy(prod1)
        t1b = timer(prod6, args=(m,v), repetitions=5,
                    comment='straight loops, psyco acclerated')
        e.add('psyco-acclerated plain loops', t1b)
    except ImportError:
        t1b = 0
        print 'psyco is not available - its time is set to 0'

    # weave: run prod7 a first time to allow weave to compile,
    # then measure CPU time:
    res = prod7(m, v)
    if res is not None:
        t1c = timer(prod7, args=(m,v), repetitions=250,
                    comment='plain C++ loops with weave')
        e.add('plain C++ loops with weave', t1c)

    # f77:
    t4 = timer(prod4, args=(m,v), repetitions=50, comment='loops in f77')
    e.add('loops in f77, prod4', t4)
    t5 = timer(prod5, args=(m,v), repetitions=50,
               comment='cache friendly loops in f77')
    e.add('cache friendly loops in f77, prod5', t5)

    # convert to f77 storage (this is important, with 50 calls there is
    # significant time spent on allocation!)
    m = asarray(m, order='Fortran')
    v = asarray(v, order='Fortran')
    t4b = timer(prod4, args=(m,v), repetitions=50,
                comment='loops in f77, no extra alloc')
    e.add('loops in f77, no extra alloc, prod4', t4b)
    t5b = timer(prod5, args=(m,v), repetitions=50,
                comment='cache friendly loops in f77, no extra alloc, prod5')
    e.add('cache friendly loops in f77, no extra alloc', t5b)

    # check if NumPy is faster in single precision:
    m = ones((n,n), float32)
    v = ones(n, float32)
    #t8 = timer(dot, args=(m,v), repetitions=50,
    #           comment='NumPy dot')
    #e.add('NumPy dot, float32', t8)
    print e
    """
Numeric

Matrix-vector product:
  straight Python loops:            223.7
  reduce/map, version 1:            184.5
  straight Python loops w/psyco     115.6
  reduce/map, version 2 (improved): 84.0
  f77, straightforward (rowwise):   11.26
  f77, cache friendly (columnwise): 9.84
  f77, straightforward, f storage:  2.47
  f77, cache friendly, f storage:   1.00
  straight C++ loops w/weave        1.6
  NumPy matrixmultiply:             1.03
  NumPy dot (Float64):              1.08
  NumPy dot (Float32):              0.76

real time: 1.0 -> 0.0124

Another run:
  straight Python loops:            292.7
  reduce/map, version 1:            242.1
  straight Python loops w/psyco     180.1
  reduce/map, version 2 (improved): 115.4
  f77, straightforward (rowwise):   10.06
  f77, cache friendly (columnwise): 8.66
  f77, straightforward, f storage:  2.40
  f77, cache friendly, f storage:   1.00
  straight C++ loops w/weave        1.9
  NumPy matrixmultiply:             1.09
  NumPy dot (Float64):              1.13
  NumPy dot (Float32):              0.67



numarray

Matrix-vector product:
  straight Python loops:            811.2
  reduce/map, version 1:            454.8
  straight Python loops w/psyco     629.1
  reduce/map, version 2 (improved): 257.8
  NumPy matrixmultiply:             1.00
  NumPy dot (Float64):              1.00
  NumPy dot (Float32):              3.58

Another run:

  straight Python loops:            1114.1
  reduce/map, version 1:            626.6
  straight Python loops w/psyco     914.7
  reduce/map, version 2 (improved): 353.4
  NumPy matrixmultiply:             1.05
  NumPy dot (Float64):              1.00
  NumPy dot (Float32):              3.47


real time: 1.0 -> 0.0134; multiply the numbers above by 0.0134/0.0124
    """
    # comment:
    # important with Cf2py intent(out,cache) w
    # alternative:   Cf2py intent(in,out,cache) w
    #               (now all alloc is controlled from Python)
    
    # verification test:
    n = 3
    m = ones((n,n)) + 3
    v = arange(1, n+1, 1)
    w1 = prod1(m, v)
    w2 = prod2(m, v)
    w3 = dot(m, v)
    e = sum(abs(w1-w2))
    if e > 1.0E-12:
        print "wrong matvec test, e=%g" % e; sys.exit(1)
    e = sum(abs(w1-w3))
    if e > 1.0E-12:
        print "wrong matvec test, e=%g" % e; sys.exit(1)

def resize_tests(n):
    """
    Compare resize vs append, and del operations, in lists and NumPy arrays.
    """
    def addelm_NumPy(n):
        a = zeros(1)
        for i in xrange(n):
            a = resize(a, (len(a)+1,))

    def addelm_list(n):
        a = [0.0]
        for i in xrange(n):
            a.append(0.0)

    def delelm_NumPy(n):
        a = zeros(n)
        for i in xrange(n):
            a = resize(a, (len(a),))
        
    def delelm_list(n):
        a = [0.0]*n
        for i in xrange(n):
            del a[0]

    # run a series of array/list lengths; resize scales poorly
    t1 = []
    t2 = []
    t3 = []
    t4 = []
    best_add = []
    best_del = []
    length = []
    while True:
        length.append(n)
        print 'n:', n
        t1.append(timer(addelm_list,  args=(n,), repetitions=200))
        t2.append(timer(addelm_NumPy, args=(n,), repetitions=1))
        t3.append(timer(delelm_list,  args=(n,), repetitions=100))
        t4.append(timer(delelm_NumPy, args=(n,), repetitions=1))
        best = min(t1[-1], t2[-1])
        if best < 0.001: best = 0.001
        best_add.append(best)
        best = min(t3[-1], t4[-1])
        if best < 0.001: best = 0.001
        best_del.append(best)
        if n < 1000: break
        n /= 2

    for lst, npa, n, best in zip(t1, t2, length, best_add):
        print 'n=%d list append: %.1f  resize: %.1f  (time=%.2f)' % \
              (n, lst/best, npa/best, best)
        #print 'scaling: list append: %e  resize: %e' % \
        #      (lst/float(n), npa/float(n))
    for lst, npa, n, best in zip(t3, t4, length, best_del):
        print 'n=%d list del: %.1f  resize: %.1f  (time=%.2f)' % \
              (n, lst/best, npa/best, best)
        #print 'scaling: list del: %e  resize: %e' % \
        #      (lst/float(n), npa/float(n))
    """
n=40000 list append: 1.0  resize: 1293.9  (time=0.06)
n=20000 list append: 1.0  resize: 402.8  (time=0.03)
n=10000 list append: 1.0  resize: 165.7  (time=0.01)
n=5000 list append: 1.0  resize: 99.3  (time=0.01)
n=2500 list append: 1.0  resize: 87.0  (time=0.00)
n=1250 list append: 1.0  resize: 80.0  (time=0.00)
    """
    
def loop_vs_NumPy_tests(n):
    """
    Compare a loop with sin or i+2 computations in pure Python and NumPy.
    """

    def py_loop1_sin(x):
        from math import sin  # scalar sin
        for i in xrange(len(x)):
            x[i] = sin(x[i])
        return x

    def py_loop2_sin(x):
        from numpy import sin  # vector sin (inefficient!)
        for i in xrange(len(x)):
            x[i] = sin(x[i])
        return x


    def I(x):
        # from math import sin # this is expensive: from 70 to 16!
        return sin(x)
    
    def py_loop3_sin(x):
        for i in xrange(len(x)):
            x[i] = I(x[i])
        return x

    def NumPy_loop_sin(x):
        from numpy import sin
        x = sin(x)
        return x


    def py_loop1_sincos_x2(x):
        from math import sin, cos, pow  # scalar sin
        for i in xrange(len(x)):
            x[i] = sin(x[i])*cos(x[i]) + x[i]**2
        return x

    def py_loop2_sincos_x2(x):
        from numpy import sin, cos
        for i in xrange(len(x)):
            x[i] = sin(x[i])*cos(x[i]) + x[i]**2
        return x

    def py_loop2b_sincos_x2(x):
        from math import sin, cos  # scalar sin
        for i in xrange(len(x)):
            x[i] = sin(x[i])*cos(x[i]) + x[i]**2
        return x

    def I2(x):
        # from math import sin # this is expensive: from 70 to 16!
        return sin(x)*cos(x) + x**2
    
    def py_loop3_sincos_x2(x):
        for i in xrange(len(x)):
            x[i] = I2(x[i])
        return x

    def py_loop4_sincos_x2(x):
        from math import sin, cos
        for i in xrange(len(x)):
            xi = x[i]
            x[i] = sin(xi)*cos(xi) + xi**2
        return x

    def NumPy_loop_sincos_x2(x):
        from numpy import sin, cos
        x = sin(x)*cos(x) + x**2
        return x


    def py_loop1_ip2(x):
        for i in xrange(len(x)):
            x[i] = i+2
        return x

    def NumPy_loop1_ip2(x):
        x = arange(2, n+2, 1)
        return x

    def NumPy_loop2_ip2(x):
        x = fromfunction(lambda i: i+2, (len(x),))
        return x

    def py_loop1_2Dsincos(x, y):
        u = zeros((len(x),len(y)))
        from math import sin as msin, cos as mcos

        def I(x, y):
            return msin(x)*mcos(y)

        # x[i], y[j]: coordinates of grid point (i,j)
        for i in xrange(len(x)):
            for j in xrange(len(y)):
                u[i,j] = I(x[i], y[j])
        return u

    def py_loop2_2Dsincos(x, y):
        # inlined expressions
        u = zeros((len(x),len(y)))
        from math import sin as msin, cos as mcos

        # x[i], y[j]: coordinates of grid point (i,j)
        for i in xrange(len(x)):
            for j in xrange(len(y)):
                u[i,j] = msin(x[i])*mcos(y[j])
        return u

    def py_loop3_2Dsincos(x, y):
        # reverse the order of traversal
        u = zeros((len(x),len(y)))
        from math import sin as msin, cos as mcos

        # x[i], y[j]: coordinates of grid point (i,j)
        for j in xrange(len(y)):
            for i in xrange(len(x)):
                u[i,j] = msin(x[i])*mcos(y[j])
        return u

    def NumPy_loop1_2Dsincos(x, y):
        xv = x[:, newaxis]
        yv = y[newaxis, :]

        def I3(x, y):
            return sin(x)*cos(y)

        u = I3(xv, yv)
        return u
        

    def py_loop1_manyarit(x):
        from math import sin, cos
        for i in xrange(len(x)):
            x[i] = sin(x[i])*cos(x[i]) + sin(2*x[i])*cos(2*x[i]) + \
                   sin(3*x[i])*cos(3*x[i]) + \
                   sin(4*x[i])*cos(4*x[i]) + sin(5*x[i])*cos(5*x[i]) 
        return x

    def NumPy_loop1_manyarit(x):
        from numpy import sin, cos, amax
        x = sin(x)*cos(x) + sin(2*x)*cos(2*x) + \
            sin(3*x)*cos(3*x) + \
            sin(4*x)*cos(4*x) + sin(5*x)*cos(5*x) 
        return x

    try:
        from call import sinloop as F77_loop_sin, \
             ip2loop as F77_loop_ip2, \
             sincosloop as F77_loop_sincos_x2, \
             gridloop2d1 as F77_loop1_2Dsincos, \
             gridloop2d2 as F77_loop2_2Dsincos, \
             gridloop2d3 as F77_loop3_2Dsincos, \
             manyarit as F77_loop_manyarit
        
        F77_loop_sin.__name__ = 'F77_loop_sin'
        F77_loop_ip2.__name__ = 'F77_loop_ip2'
        F77_loop_sincos_x2.__name__ = 'F77_loop_sincos_x2'
        F77_loop_manyarit.__name__ = 'F77_loop_manyarit'
        F77_loop1_2Dsincos.__name__ = 'F77_loop1_2Dsincos'
        F77_loop2_2Dsincos.__name__ = 'F77_loop2_2Dsincos'
        F77_loop3_2Dsincos.__name__ = 'F77_loop3_2Dsincos'
    except Exception, msg:
        print 'import error (from call import ...):', msg
        print 'Run f2py -c -m call call.f'
        sys.exit(1)


    # 2D grid:
    print '\n\n'
    m0 = sqrt(n)
    #m0 = 1600
    for j in (4, 2, 1):
        m = m0/j
        print '\n\ninitializing a %dx%d array\n' % (m,m)
        x = seq(0, 1, 1/float(m-1))
        y = x.copy()
        u = zeros((len(x), len(y)))
        t1 = timer(py_loop3_2Dsincos, args=(x,y), repetitions=1*j)
        t1 = timer(py_loop2_2Dsincos, args=(x,y), repetitions=1*j)
        t1 = timer(py_loop1_2Dsincos, args=(x,y), repetitions=1*j)
        print 'pure Python loop:%d u[i,j]=sin(x[i])*cos(y[j]):' % size(u), t1
        from scitools.numpytools import sin, cos  # ensure vectorized versions
        t2 = timer(NumPy_loop1_2Dsincos, args=(x,y), repetitions=20*j)
        print 'NumPy:%d u=sin(x)*cos(y):' % size(u), t2
        import call
        u = asarray(u, order='Fortran')
        t3a = timer(F77_loop1_2Dsincos, args=(u,x,y), repetitions=20*j,
                    comment='call I1')
        t3b = timer(F77_loop2_2Dsincos, args=(u,x,y), repetitions=20*j,
                    comment='inline')
        t3c = timer(F77_loop3_2Dsincos, args=(u,x,y), repetitions=20*j,
                    comment='3 loops')
        #v1 = NumPy_loop1_2Dsincos(x,y)
        #v2 = F77_loop_2Dsincos(u, x, y)
        #print 'difference v1-v2', size(v1-v2), amax(v1-v2), amin(v1-v2)
        print 'corresponing Fortran loop:', t3a, t3b, t3c
        t3 = min(t3a, t3b, t3c)
        print 'summary:', t1/t3, t2/t3, '1.0'
        print 'NumPy is', t1/t2, 'times faster than pure Python'

    x = seq(0, 1, 1/float(n-1))

    # 5 combinations of sin(x)*cos(x)
    # (NumPy vectorized code is less efficient than Fortran now because
    # there are so many calls to C functions and 
    print '\n\n'
    t1 = timer(py_loop1_manyarit, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d x[i]=5 sin(x[i])*cos(x[i]) terms:' % n, t1
    print 'x max:', amax(x)
    from scitools.numpytools import sin, cos
    t2 = timer(NumPy_loop1_manyarit, args=(x,), repetitions=4)
    print 'corresponing NumPy expression x=5 terms sin(x)*cos(x):', t2
    t3 = timer(F77_loop_manyarit, args=(x,), repetitions=20)
    print 'corresponing Fortran loop:', t3
    print 'NumPy is %d times faster than plain loops' % (t1/t2)
    print 'summary: %g %g %g' % (t1/t3, t2/t3, 1.0)

    
    # sin(x):
    print '\n\n'
    t1 = timer(py_loop1_sin, args=(x.tolist(),), repetitions=1)
    print 'pure Python loop 1:%d x[i]=sin(x[i]), x is list, scalar math.sin:' % n, t1
    t1 = timer(py_loop1_sin, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d x[i]=sin(x[i]), x is array, scalar math.sin:' % n, t1
    t1b = timer(py_loop2_sin, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d x[i]=sin(x[i]), NumPy vector sin:' % n, t1b
    t2 = timer(NumPy_loop_sin, args=(x,), repetitions=20)
    print 'corresponing NumPy expression x=sin(x):', t2
    from math import sin  # scalar sin
    t1c = timer(py_loop3_sin, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d x[i]=I(x[i]), I is sin(x):' % n, t1c
    t3 = timer(F77_loop_sin, args=(x,), repetitions=20)
    print 'corresponing Fortran loop:', t3
    print 'summary:', t1/t3, t2/t3, '1.0'
    print 'NumPy is', t1/t2, 'times faster than pure Python'
    print 'NumPy sin (instead of math.sin):', t1b/t3
    print 'calling I(x[i]):', t1c/t3

    # sin(x)*cos(x) + x**2
    print '\n\n'
    t1 = timer(py_loop2b_sincos_x2, args=(x.tolist(),), repetitions=1)
    print 'pure Python loop 1:%d x[i]=sin(x[i])*cos(x[i])+x[i]*x[i]:' % n, t1
    t1 = timer(py_loop2_sincos_x2, args=(x.tolist(),), repetitions=1)
    print 'pure Python loop 1:%d x[i]=sin(x[i])*cos(x[i])+x[i]*x[i] with NumPy sin, cos, power:' % n, t1
    t1 = timer(py_loop1_sincos_x2, args=(x.tolist(),), repetitions=1)
    print 'pure Python loop 1:%d x[i]=sin(x[i])*cos(x[i])+x[i]**2, x is list, scalar math.sin:' % n, t1
    t1 = timer(py_loop1_sincos_x2, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d x[i]=sin(x[i])*cos(x[i])+x[i]**2, x is array, scalar math.sin:' % n, t1
    t2 = timer(NumPy_loop_sincos_x2, args=(x,), repetitions=20)
    print 'corresponing NumPy expression x=sin(x)*cos(x)+x**2:', t2
    from math import sin, cos  # scalar sin, cos
    t1c = timer(py_loop3_sincos_x2, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d x[i]=I2(x[i]), I is sin(x)*cos(x[i])+x**2:' % n, t1c
    t1b = timer(py_loop4_sincos_x2, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d xi=x[i]; x[i]=sin(xi)*cos(xi)+xi**2:' % n, t1b
    t3 = timer(F77_loop_sincos_x2, args=(x,), repetitions=20)
    print 'corresponing Fortran loop:', t3
    print 'summary:', t1/t3, t2/t3, '1.0'
    print 'NumPy is', t1/t2, 'times faster than pure Python'
    print 'xi=x[i] trick:', t1b/t3, t1b
    print 'calling I2(x[i]):', t1c/t3

    # i+2:
    print '\n\n'
    t1 = timer(py_loop1_ip2, args=(x.tolist(),), repetitions=1)
    print 'pure Python loop 1:%d x[i]=i+2, x is list:' % n, t1
    t1 = timer(py_loop1_ip2, args=(x,), repetitions=1)
    print 'pure Python loop 1:%d x[i]=i+2, x is array:' % n, t1
    t2 = timer(NumPy_loop1_ip2, args=(x,), repetitions=20)
    print 'corresponing NumPy expression x=arange:', t2
    t2 = timer(NumPy_loop1_ip2, args=(x,), repetitions=20)
    print 'corresponing NumPy expression x=fromfunction(lambda; i i+2):', t2
    t3 = timer(F77_loop_ip2, args=(x,), repetitions=20)
    print 'corresponing Fortran loop:', t3
    print 'summary:', t1/t3, t2/t3, '1.0'
    print 'NumPy is', t1/t2, 'times faster than pure Python'

    
def exception_test(n):
    """
    Test the efficiency of try-except vs if-else in different
    settings (frequent vs seldom try failure).
    """
    
    e = EfficiencyTable('try-except vs if-else in math expression')
    setup1 = """
from math import sqrt
def sqrt_if_xgtz(x):
    try:
        return sqrt(x)
    except:
        return 0.0
"""
    setup2 = """
from math import sqrt
def sqrt_if_xgtz(x):
    if x > 0:
        return sqrt(x)
    else:
        return 0.0
 """
    setup3 = """
from math import sqrt
""" # use only sqrt (no test)
    t1 = timeit.Timer('sqrt_if_xgtz(-1.1)', setup=setup1).timeit(n)
    e.add('try fails', t1)
    t2 = timeit.Timer('sqrt_if_xgtz(-1.1)', setup=setup2).timeit(n)
    e.add('if test is true', t2)
    t3 = timeit.Timer('sqrt_if_xgtz(1.1)', setup=setup1).timeit(n)
    e.add('except does not happen', t3)
    t4 = timeit.Timer('sqrt_if_xgtz(1.1)', setup=setup2).timeit(n)
    e.add('if test is false', t4)
    t5 = timeit.Timer('sqrt(1.1)', setup=setup3).timeit(n)
    e.add('no test, just sqrt call', t5)
    print e

    e = EfficiencyTable('try-except vs if-else for key in dict test')
    bigdict = {}
    for i in range(1000):
        bigdict[str(i)] = i
    bigdict = 'd=' + str(bigdict)

    setup1 = bigdict + """
def lookup(x):
    try:
        return x['f']
    except:
        return 0.0
"""
    setup2 = bigdict + """
def lookup(x):
    if 'f' in x:
        return x['f']
    else:
        return 0.0
"""
    setup3 = bigdict + """
def lookup(x):
    return x.get('f', 0.0)
"""
    t1 = timeit.Timer('lookup(d)', setup=setup1).timeit(n)
    e.add('try-except look up', t1)
    t2 = timeit.Timer('lookup(d)', setup=setup2).timeit(n)
    e.add('if key in dict look up', t2)
    t3 = timeit.Timer('lookup(d)', setup=setup3).timeit(n)
    e.add('dict.get function look up', t3)
    print e
    
def iterator_tests(n):
    """
    Test the efficiency of iterators over 3D grids.
    """

    # first test loops with array loop-up:
    def plainloops(u, shape):
        for i in xrange(shape[0]):
            for j in xrange(shape[1]):
                for k in xrange(shape[2]):
                    u[i,j,k] = u[i,j,k] + 1
        return u

    def griditer_scalar(u, g):
        for i, j, k in g.iter('all', False):
            u[i,j,k] = u[i,j,k] + 1
        return u

    def griditer_slices(u, g):
        for islice, jslice, kslice in g.iter('all', True):
            for i in xrange(islice.start, islice.stop):
                for j in xrange(jslice.start, jslice.stop):
                    for k in xrange(kslice.start, kslice.stop):
                        u[i,j,k] = u[i,j,k] + 1

        return u

    # empty loops:
    def plainloops_empty(u, shape):
        for i in xrange(shape[0]):
            for j in xrange(shape[1]):
                for k in xrange(shape[2]):
                    pass

    def griditer_scalar_empty(u, g):
        for i, j, k in g.iter('all', False):
            pass

    def griditer_slices_empty(u, g):
        for islice, jslice, kslice in g.iter('all', True):
            for i in xrange(islice.start, islice.stop):
                for j in xrange(jslice.start, jslice.stop):
                    for k in xrange(kslice.start, kslice.stop):
                        pass

    e1 = EfficiencyTable('iterating over a 3D %dx%dx%dx grid; '\
                         'loops with array look up' % (n,n,n))
    from scitools.BoxGrid import UniformBoxGrid
    g = UniformBoxGrid(x=(0,1), nx=n-1, y=(0,1), ny=n-1, z=(0,1), nz=n-1)
    u = zeros((n,n,n))
    t1 = timer(plainloops, args=(u, g.shape), repetitions=1)
    e1.add('three plain nested loops', t1)
    t2 = timer(griditer_scalar, args=(u, g), repetitions=1)
    e1.add('grid iterator, scalar version', t2)
    t3 = timer(griditer_slices, args=(u, g), repetitions=1)
    e1.add('grid iterator, slices used for plain loops', t3)
    print e1

    e2 = EfficiencyTable('iterating over a 3D %dx%dx%dx grid; '\
                         'empty loops' % (n,n,n))
    t1 = timer(plainloops_empty, args=(u, g.shape), repetitions=1)
    e2.add('three plain nested loops', t1)
    t2 = timer(griditer_scalar_empty, args=(u, g), repetitions=1)
    e2.add('grid iterator, scalar version', t2)
    t3 = timer(griditer_slices_empty, args=(u, g), repetitions=1)
    e2.add('grid iterator, slices used for plain loops', t3)
    print e2

    try:
        import psyco
        plainloops_psyco = psyco.proxy(plainloops)
        griditer_scalar_psyco = psyco.proxy(griditer_scalar)
        griditer_slices_psyco = psyco.proxy(griditer_slices)

        e3 = EfficiencyTable('iterating over a 3D %dx%dx%d grid; '\
                             'array look-up with psyco acceleration' % (n,n,n))
        t1 = timer(plainloops_psyco, args=(u, g.shape), repetitions=1)
        e3.add('three plain nested loops', t1)
        t2 = timer(griditer_scalar_psyco, args=(u, g), repetitions=1)
        e3.add('grid iterator, scalar version', t2)
        t3 = timer(griditer_slices_psyco, args=(u, g), repetitions=1)
        e3.add('grid iterator, slices used for plain loops', t3)
        print e3
        print 'gain of using psyco:', e1.best/e3.best
    except ImportError:
        print 'psyco is not available!'

    # test simple generators:
    def gen1(n):
        for i in xrange(n):
            yield i

    def it1(n):
        for i in gen1(n):  # yield overhead
            pass

    def it2(n):
        for i in xrange(n):
            pass

    e4 = EfficiencyTable('generator vs plain loop')
    n = n**4
    t1 = timer(it1, args=(n,), repetitions=1)
    e4.add('for i in generator_function', t1)
    t2 = timer(it2, args=(n,), repetitions=1)
    e4.add('for i in xrange(n)', t2)
    print e4

    
def stdfunc_tests(n):
    """
    Test the efficiency of standard functions like sqrt from
    various modules: math, cmath, Numeric, numarray, numpy,
    numpy.lib.scimath (scipy).

    Motivation:
    The numpy.lib.scimath sqrt function (and other functions too)
    are very convenient since sqrt of a negative number gives
    complex output, otherwise one gets real output. One could
    therefore always import the standard math functions from
    numpy.lib.scimath (or scipy), but how much does this convenience cost?
    """
    # From a MacBook Pro:
    """
********************************************************************************
Test of 1000000 scalar sqrt function evaluations
********************************************************************************
reference CPU time based on the experiment
   "from math import sqrt; r = sqrt(1.1)"
with CPU time:
  0.33823919296264648

from math import sqrt;           r = sqrt(1.1)                   |     1.00
from cmath import sqrt;          r = sqrt(1.1)                   |     1.27
import math;                     r = math.sqrt(1.1)              |     1.39
import cmath;                    r = cmath.sqrt(1.1)             |     1.63
from numpy import sqrt;          r = sqrt(1.1)                   |    12.75
import numpy;                    r = numpy.sqrt(1.1)             |    13.41
from numarray import sqrt;       r = sqrt(1.1)                   |    14.05
import numarray;                 r = numarray.sqrt(1.1)          |    14.62
from Numeric import sqrt;        r = sqrt(1.1)                   |    23.13
import Numeric;                  r = Numeric.sqrt(1.1)           |    23.52
from numpy.lib.scimath import sqrt;  r = sqrt(1.1)               |   127.77
import numpy.lib.scimath;        r = numpy.lib.scimath.sqrt(1.1) |   130.33
    """
    
    msg = 'Test of %d scalar sqrt function evaluations' % n
    e = EfficiencyTable(msg)
    print msg

    function = 'sqrt'
    for module in 'math', 'cmath', 'numpy', 'numpy.lib.scimath', \
        'Numeric', 'numarray', 'scipy':
        from_import = 'from %s import %s' % (module, function)
        full_import = 'import %s' % module
        from_call = 'r = %s(1.1)' % function
        full_call = 'r = %s.%s(1.1)' % (module, function)
        try:
            t = timeit.Timer(from_call, setup=from_import).timeit(n)
            e.add('%-30s;  %-20s' % (from_import, from_call), t)
            t = timeit.Timer(full_call, setup=full_import).timeit(n)
            e.add('%-30s;  %-20s' % (full_import, full_call), t)
        except ImportError:
            print 'Module %s is not installed - skipping its %s function' % \
                  (module, function)
    print e
    
        
def grep_tests(filesize):
    """
    Test the efficiency of a grep operation on a file with
    size filesize megabytes.
    Compare Python, Perl, and Unix grep (in C).
    """
    from generate_text import generate
    generate(filesize, filename='tmp.dat')

    # run os.system commands (on Windows, spaces in the %scripting%
    # path will make os.system fail, better to add dirs to PATH):
    py = os.path.join(os.environ['scripting'],'exercises','grep.py')
    pl_nice = os.path.join(os.environ['scripting'],'src','perl','grep1.pl')
    pl_dollar_underscore = \
        os.path.join(os.environ['scripting'],'src','perl','grep2.pl')
    pl_grep = os.path.join(os.environ['scripting'],'src','perl','grep4.pl')
    unix = 'grep -H -n'
    pattern = '000000761'
    pattern = repr(r'\s+0+000[0AZBfq][7_/]61\s+')  # add quotes for system cmd
    timer_system(py + ' ' + pattern + ' tmp.dat', comment='plain Python:')
    timer_system(pl_nice + ' ' + pattern + ' tmp.dat', comment='plain Perl:')
    timer_system(pl_dollar_underscore + ' ' + pattern + ' tmp.dat', comment='Perl w/$_:')
    timer_system(pl_grep + ' ' + pattern + ' tmp.dat', comment='Perl grep:')
    timer_system(unix + ' ' + pattern + ' tmp.dat', comment='Unix grep:')
    os.remove('tmp.dat')

def _laptop1():
    """Tests with problem sizes."""
    scale_factor = 1
    for test_tp in sys.argv[1:]:
        print '\nMachine info:'
        import pprint; pprint.pprint(hardware_info())
        print '\n\n'
        if test_tp == 'allocate':
            allocate_tests(int(10000000*scale_factor)) 
        elif test_tp == 'range':
            memory = 512000000
            vector_length = int(memory/8.0/2*0.9)/10
            range_tests(vector_length)
        elif test_tp == 'call':
            #call_tests(5000000, 0.1, -0.1, 1.0)
            #call_tests(500000, 0.1, -0.1, 1.0)
            call_tests(int(50000*scale_factor), 0.1, -0.1, 1.0)
        elif test_tp == 'flops':
            flop_tests(int(20000000*scale_factor))
        elif test_tp == 'matrixfill_f77':
            matrixfill_tests('F77', int(1000*scale_factor))
        elif test_tp == 'matrixfill_Cpp':
            matrixfill_tests('C++', int(1000*scale_factor))
        elif test_tp == 'matrixfill':
            matrixfill_tests(('F77','C++'), int(1000*scale_factor))
        elif test_tp == 'grep':
            grep_tests(120)
        elif test_tp == 'vectorization':
            loop_vs_NumPy_tests(int(1000000*scale_factor))
        elif test_tp == 'exception':
            exception_test(int(1000000*scale_factor))
        elif test_tp == 'matvec':
            matvec_prod_tests(int(2000*scale_factor))
        elif test_tp == 'resize':
            resize_tests(int(40000*scale_factor))
        elif test_tp == 'type':
            type_tests(int(1000000*scale_factor), int(1000000*scale_factor)) 
        elif test_tp == 'size':
            size_tests(int(1000000*scale_factor), int(1000000*scale_factor)) 
        elif test_tp == 'iterator':
            iterator_tests(int(60*scale_factor))
        elif test_tp == 'factorial':
            factorials(int(5*scale_factor))
        elif test_tp == 'sqrt':
            stdfunc_tests(int(1000000*scale_factor))
        elif test_tp == 'tripleloops':
            triple_numpy_loops(30, 10)
        elif test_tp == '--Numeric' or test_tp == '--numarray':
            pass
        else:
            print test_tp, 'not implemented'
    
if __name__ == '__main__':
    if len(sys.argv) < 2:
        print "Usage: %s test-type\ntypes: allocate range call "\
              "flops matrixfill_f77 matrixfill_Cpp vectorization "\
              "exception iterator resize type grep factorial" \
              % sys.argv[0]
        sys.exit(1)
    _laptop1()
