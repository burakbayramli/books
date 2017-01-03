#!/usr/bin/env python
"""
Some simple examples on using ScientificPython.
"""
import sys, os
from numpy import *


print 'testing numbers with physical units:'
from Scientific.Physics.PhysicalQuantities import PhysicalQuantity
q = PhysicalQuantity # short hand
m = q(12, 'kg')        # number, dimension
a = q('0.88 km/s**2')  # alternative syntax (string)
F = m*a
print 'arithmetics with PhysicalQuantity instances:', F
F = F.inBaseUnits()
print 'converted to basic SI units:', F
F.convertToUnit('MN')  # convert to Mega Newton
print 'converted to Mega Newton:', F
F = F + q(0.1, 'kPa*m**2')  # kilo Pascal m^2
print 'adding 0.1 kPa m^2:', F
print 'str(F):', str(F)  # '0.010759999999999999 MN'
value = float(str(F).split()[0])
print 'extract float value:', value
print F.inBaseUnits()
print '0 Celcius in Farenheit:', q('0 degC').inUnitsOf('degF')

print '\n\nTesting automatic derivatives:'
from Scientific.Functions.Derivatives import DerivVar as D
def somefunc(x, y, z):
    return 3*x - y + 10*z**2

x = D(2, index=0)     # variable no. 0
y = D(0, index=1)     # variable no. 1
z = D(0.05, index=2)  # variable no. 2
r = somefunc(x, y, z)
print '(function value, [d/dx, d/dy, d/dz]):\n', r
# (6.0250000000000004, [3.0, -1.0, 1.0])

print '(sin(0), [cos(0)]):', sin(D(0.0)) # (0.0, [1.0])
x = D(1, order=3)
print '3rd order derivatives of x^3:',  x**3
# (1, [3], [[6]], [[[6]]])  # 0th, 1st, 2nd, 3rd derivative

x = D(10, index=0, order=2)
y = D(0, index=1, order=2)
z = D(1, index=2, order=2)
r = somefunc(x, y, z)
print r
# (40, [3, -1, 20], [[0, 0, 0], [0, 0, 0], [0, 0, 20]])
print 'd^2(somefunc)/dzdx:', r[2][2][0] # 0
print 'd^2(somefunc)/dz^2:', r[2][2][2] # 20

print '\n\ntesting interpolation:'
from Scientific.Functions.Interpolation \
     import InterpolatingFunction as Ip
t = linspace(0, 10, 101)
v = sin(t)
vi = Ip((t,), v)
# interpolate and compare with exact result:
print 'interpolated:', vi(5.05), ' exact:', sin(5.05)  
# interpolate the derivative of v:
vid = vi.derivative()
print 'interpolated derivative:', vid(5.05), ' exact:', cos(5.05)
# compute the integral of v over all t values:
print 'definite integral:', vi.definiteIntegral(), \
      ' exact:', -cos(t[-1]) - (-cos(t[0]))

# add path to Grid2D (for testing interpolation on a 2D grid):
sys.path.insert(0, os.path.join(os.environ['scripting'],
                                'src', 'py', 'examples'))
from Grid2D import Grid2D
g = Grid2D(dx=0.1, dy=0.2)
f = g(lambda x, y: sin(pi*x)*sin(pi*y))
fi = Ip((g.xcoor, g.ycoor), f)
# interpolate at (0.51,0.42) and compare with exact result:
print 'interpolation in 2D grid:', fi(0.51,0.42), \
      ' exact value:', sin(pi*0.51)*sin(pi*0.42)
# (0.94640171438438569, 0.96810522380784525)



print '\n\ntesting nonlinear least squares:'
def error_model(parameters, gridsize):
    C, a, D, b = parameters
    dx, dt = gridsize
    e = C*dx**a + D*dt**b
    return e

# generate 7 data points:
data = []
import random as random_number
random_number.seed(11)
C = 1; a = 2; D = 2; b = 1; p = (C, a, D, b)
dx = 0.5; dt = 1.0
for i in range(7):
    dx /= 2;  dt /= 2 
    e = error_model(p, (dx, dt))   # use the model to generate data
    e += random_number.gauss(0, 0.01*e)  # perturb data randomly
    data.append( ((dx,dt), e) )

from Scientific.Functions.LeastSquares import leastSquaresFit
parameter_guess = (1, 2, 2, 1)  # exact guess...
r = leastSquaresFit(error_model, parameter_guess, data)
print r
# try different initial guesses:
for i in range(6):
    # exact p +/- [1-6]*(randomly chosen -1 or 1):
    pg = array(p, float) + i*0.05*sign(random_number.uniform(-1,1))*ones(4)
    r = leastSquaresFit(error_model, pg, data)
    print 'guess:', str(pg)
    deviation = array(p) - array(r[0])
    print '     deviation:', deviation
    
print '\n\nTesting statistical computations:'
import Scientific.Statistics as S
nsamples = 100000
data = random.normal(1.0, 0.5, nsamples)
mean = S.mean(data)
stdev = S.standardDeviation(data)
median = S.median(data)
skewness = S.skewness(data)
kurtosis = S.kurtosis(data)
print 'mean=%.2f  standard deviation=%.2f  skewness=%.1f '\
      'median=%.2f, kurtosis=%.2f' % \
      (mean, stdev, skewness, median, kurtosis)

from Scientific.Statistics.Histogram import Histogram
h = Histogram(data, 50)  # use 50 bins between min & max samples
h.normalizeArea()        # make probabilities in histogram
from scitools.easyviz import *
plot(h.getBinIndices(), h.getBinCounts())

