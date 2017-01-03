#!/usr/bin/env python
import sys, os, time, numpy, scipy

def somefunc1(x):  # function of a scalar argument
    if x < 0:
        r = 0
    else:
        r = scipy.sin(x)
    return r

def somefunc2(x): # function of a scalar argument
    if x < 0:
        r = 0
    else:
        r = math.sin(x)
    return r

def somefunc3(x): # function of a scalar argument
    if x < 0:
        r = 0
    else:
        r = numpy.sin(x)
    return r

def somefunc_NumPy2(x):
    # vectorized function by hand:
    lt0_indices = less(x, 0)  # find all indices i where x[i]<0
    r = sin(x)
    # insert 0 for all elements if x[i]<0:
    r = where(lt0_indices, 0.0, r)
    return r


#--------------- demonstrate some SciPy functionality ----------------
from scipy import *

def integrate_func():
    def myfunc(x):
        return sin(x)
    result, error = integrate.quad(myfunc, 0, pi)
    print result, error

    def myfunc(x, a, b):
        return a + b*sin(x)
    a=0; b=1
    result, error = integrate.quad(myfunc, 0, pi, args=(a,b), epsabs=1.0e-9)
    print result, error

    
class Oscillator:
    """Implementation of the oscillator code using SciPy."""
    def __init__(self, **kwargs):
        """Initialize parameters from keyword arguments."""
        self.p = {'m': 1.0, 'b': 0.7, 'c': 5.0, 'func': 'y',
                  'A': 5.0, 'w': 2*pi, 'y0': 0.2,
                  'tstop': 30.0, 'dt': 0.05}
        self.p.update(kwargs)
        
    def scan(self):
        """
        Read parameters from standard input in the same
        sequence as the F77 oscillator code.
        """
        for name in 'm', 'b', 'c', 'func', 'A', 'w', \
                'y0', 'tstop', 'dt':
            if name == 'func':  # expect string
                self.p['func'] = sys.stdin.readline().strip()
            else:
                self.p[name] = float(sys.stdin.readline())

    def solve(self):
        """Solve ODE system."""
        # mapping: name of f(y) to Python function for f(y):
        self._fy = {'y': lambda y: y, 'siny': lambda y: sin(y),
                    'y3': lambda y: y - y**3/6.0}
        # set initial conditions:
        self.y0 = [self.p['y0'], 0.0]
        # call SciPy solver:
        from scitools.numpyutils import seq
        self.t = seq(0, self.p['tstop'], self.p['dt'])

        from scipy.integrate import odeint
        self.yvec = odeint(self.f, self.y0, self.t)

        self.y = self.yvec[:,0]  # y(t)
        # write t and y(t) to sim.dat file:
        f = open('sim.dat', 'w')
        for y, t in zip(self.y, self.t):
            f.write('%g %g\n' % (t, y))
        f.close()

    def f(self, y, t):
        """Right-hand side of 1st-order ODE system."""
        A, w, b, c, m = [p[k] for k in 'A', 'w', 'b', 'c', 'm']
        f = self._fy[self.p['func']]
        return [y[1], (A*cos(w*t) - b*y[1] - c*f(y[0]))/m]


def test_Oscillator(dt=0.05):
    s = Oscillator(m=5, dt=dt)
    t1 = os.times()
    s.solve()
    t2 = os.times()
    print 'CPU time of odeint:', t2[0]-t1[0] + t2[1]-t1[1]

    # compare with the oscillator program:
    cmd = './simviz1.py -noscreenplot -case tmp1'
    for option in s.p:  # construct command-line options
        cmd += ' -'+option + ' ' + str(s.p[option])
    t3 = os.times()
    os.system(cmd)
    t4 = os.times()
    print 'CPU time of oscillator:', t4[2]-t3[2] + t4[3]-t3[3]
    # plot:
    from scitools.filetable import readfile
    t, y = readfile(os.path.join('tmp1','sim.dat'))
    from scitools.easyviz import *
    plot(t, y, 'r-', s.t, s.y, 'b-', legend=('RK2', 'LSODE'))
    hardcopy('tmp.ps')

def statistics():
    pd = stats.norm(loc=1, scale=0.5)  # normal distribution N(1,0.5)
    n=10000
    r = pd.rvs(n) # random variates
    import RandomArray
    r = RandomArray.normal(1, 0.1, n)
    s = stats.stats
    print pd.stats()
    print 'mean=%g stdev=%g skewness=%g kurtosis=%g' % \
          (s.mean(r), s.variation(r), s.skew(r), s.kurtosis(r))
    bin_counts, bin_min, min_width, noutside = s.histogram(r, numbins=50)


def linear_algebra():
    # init:
    n = 4
    A = zeros(n*n, float); A.shape = (n,n)
    x = zeros(n, float)
    b = zeros(n, float)

    for i in range(n):
        x[i] = i/2.0       # some prescribed solution
        for j in range(n):
            A[i,j] = 2.0 + float(i+1)/float(j+i+1)
    b = dot(A,x)  # adjust rhs to fit x

    y = linalg.solve(A, b)
    if allclose(x, y, atol=1.0E-12, rtol=1.0E-12):
        print 'correct solution'
    else:
        print 'wrong solution', x, y


# test part:
if __name__ == '__main__':
    if len(sys.argv) <= 1:
        print 'Usage: SciPy.py integrate | Oscillator dt | statistics'
        sys.exit(1)
    test = sys.argv[1]
    if test == 'integrate':
        integrate_func()
    elif test == 'Oscillator':
        test_Oscillator(dt=float(sys.argv[2]))
    elif test == 'statistics':
        statistics()
    elif test == 'linalg':
        linear_algebra()
    else:
        print test, 'not implemented'
    

