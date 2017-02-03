"""
Some difficult plot cases.
"""

# Heaviside function
def H(x):
    return (0 if x < 0 else 1)

def H_loop(x):
    r = zeros(len(x), type(x[0]))
    for i in xrange(len(x)):
        r[i] = H(x[i])
    return r

from scitools.std import *
x = linspace(-10, 10, 5)

try:
    r = H(x)
except Exception, e:
    print 'H(vector) did not work:', e

H_vec = vectorize(H)

# Handwritten vectorized version
def Hv(x):
    return where(x < 0, 0.0, 1.0)

h = Hv(x)
h_control1 = H_vec(x)
h_control2 = H_loop(x)
print x
print 'Handvectorized H:', h
print 'Loop-based H:', h_control2
print 'numpy.vectorized H:', h_control1

plot(x, h, axis=[x[0], x[-1], -0.1, 1.1], hardcopy='tmp1.eps')

x2 = linspace(-10, 10, 50)
print x2
h2 = Hv(x2)
plot(x, h, 'r', x2, h2, 'b', label=('step: 5', 'step: 0.5'),
     axis=[x[0], x[-1], -0.1, 1.1], hardcopy='tmp2.eps')

# Specialized plot
plot([-10, 0, 0, 10], [0, 0, 1, 1],
     axis=[x[0], x[-1], -0.1, 1.1],
     hardcopy='tmp3.eps')

# Hat function
def N(x):
    if x < 0:
        return 0.0
    elif 0 <= x < 1:
        return x
    elif 1 <= x < 2:
        return 2 - x
    elif x >= 2:
        return 0.0

N_vec = vectorize(N)

# This one does not work since a < x < b conditions do not work
# for numpy arrays:
def Nv0(x):
    r = where(x < 0, 0.0, x)
    r = where(0 <= x < 1, x, r)
    r = where(1 <= x < 2, 2-x, r)
    r = where(x >= 2, 0.0, r)
    return r

# Remedy: use operator.add_
import operator

def Nv(x):
    r = where(x < 0, 0.0, x)
    condition = operator.and_(0 <= x, x < 1)
    r = where(condition, x, r)
    condition = operator.and_(1 <= x, x < 2)
    r = where(condition, 2-x, r)
    r = where(x >= 2, 0.0, r)
    return r

def Nv2(x):
    r = x.copy()  # avoid modifying x in-place
    r[x < 0.0] = 0.0
    condition = operator.and_(0 <= x, x < 1)
    r[condition] = x[condition]
    condition = operator.and_(1 <= x, x < 2)
    r[condition] = 2-x[condition]
    r[x >= 2] = 0.0
    return r

x = linspace(-2, 4, 6)
n_vec = N_vec(x)
nv = Nv(x)
nv2 = Nv2(x)
print x
print n_vec
print nv
print nv2
plot(x, nv, 'r', axis=[x[0], x[-1], -0.1, 1.1],
     hardcopy='tmp4.eps')
x2 = linspace(-2, 4, 7)
# or x2 = [-2, 0, 1, 2, 4]

plot(x, nv, 'b', x2, Nv(x2), 'r',
     axis=[x[0], x[-1], -0.1, 1.1],
     hardcopy='tmp5.eps')

#x = [-2, 0, 1, 2, 4]
#plot(x, N_vec(x))

# Rapidly varying function
def f(x):
    return sin(1.0/x)

x1 = linspace(-1, 1, 10)
x2 = linspace(-1, 1, 1000)
x3 = linspace(-1, 1, 100000)
for x in x1, x2, x3:
    print 'plotting sin(1/x) with %d points' % len(x)
    plot(x, f(x), 'r', label='%d points' % len(x),
         hardcopy='tmp_x%d' % len(x))



    
