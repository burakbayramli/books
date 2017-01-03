#!/usr/bin/env python
"""
Demonstrate basic usage of Numerical Python.

This script performs the same steps as NumPy_basics.py, but
the script starts with a

from scitools.numpytools import *

instead of importing from numpy only. With numpytools, it is
possible to switch between all three implementations of Numerical
Python: Numeric, numarray, and numpy. The unified syntax employs
the old Numeric syntax (e.g., to access the linear algebra module,
one applies the name LinearAlgebra, even if numpy is chosen as the
underlying implementation).
"""

# unified interface to Numeric, numarray and numpy:
from scitools.numpytools import *

n=10

# create an array a of length n, with zeroes and
# double precision float type:
a = zeros(n, Float)
print 'zeros(n, Float):', type(a), type(a[0]), a
a = zeros(n) # becomes array of integers
print 'zeros(n)', type(a), a

x = linspace(-5, 5, 11)
print 'linspace(-5, 5, 11)', type(x), type(x[0]), x

# arange is possible but not recommended
x = arange(-5, 5, 1, Float)
# note: round-off errors may prevent the last element
# in x from being equal to the upper bound 5
x = arange(-5, 5.1, 1)  # ensures that 5 is in x
print 'arange(-5, 5.1, 1)', type(x), type(x[0]), x
# better:
from scitools.numpyutils import seq
x = seq(-5, 5, 1)
print 'seq(-5, 5, 1)', type(x), type(x[0]), x

# it is trivial to make accompanying y values:
y = sin(x/2.0)*3.0
print 'y = sin(x/2.0)*3.0:', type(y), type(y[0]), y

# create a NumPy array of a Python list:
pl = [0, 1.2, 4, -9.1, 5, 8]
a = array(pl, Float)
print 'pl = [0, 1.2, 4, -9.1, 5, 8]; '\
      'array(pl, Float)', type(a), a
# from nested Python list to NumPy arrays and back again:
x = [0, 0.5, 1]; y = [-6.1, -2, 1.2]  # lists
a = array([x, y])  # form 2x3 array (x and y as rows)
# turn 1st row to Python list and use index to locate an entry:
i = a[0,:].tolist().index(0.5)
print 'locate i as index for 0.5 in a', a, 'i:', i

# initialization from a function:
def myfunc(i, j):
    return (i+1)*(j+4-i)

# make 3x6 array where a[i,j] = myfunc(i,j):
a = fromfunction(myfunc, (3,6))
print 'fromfunction(myfunc, (3,6))', a, type(a[0,0])

# make a one-dim. array of length n:
n = 1000000
def myfunc2(i):  return sin(i*0.0001)
print '\ncreating arrays of length %5.0E ... ' % (float(n))
import time; t1 = time.clock()
a = fromfunction(myfunc2, (n,))
t2 = time.clock()
cpu_fromfunction = t2 - t1
# alternative initialization via linspace and sin:
a = linspace(1, n, n+1); a = sin(a*0.0001)
cpu_arange_sin = time.clock() - t2
print 'fromfunction took', cpu_fromfunction, \
      's and arange&sin took', cpu_arange_sin, 's for length', n

# indexing:
a = array([0, 1.2, 4, -9.1, 5, 8])
a.shape = (2,3) # turn a into a 2x3 matrix

print a[0,1]     # print entry (0,1)
i=1; j=0
a[i,j] = 10      # assignment to entry (i,j)
print a[:,0]     # print first column
a[:,:] = 0       # set all elements of a equal to 0

a = linspace(0, 29, 30)
a.shape = (5,6)
print a
print a[1:3,:-1:2]    # a[i,j] for i=1,2 and j=0,2,4
print a[::3,2:-1:2]   # a[i,j] for i=0,3 and j=2,4
i = slice(None, None, 3);  j = slice(2, -1, 2)
print a[i,j]


a = array([0, 1.2, 4, -9.1, 5, 8])
a.shape = (2,3) # turn a into a 2x3 matrix

# traverse array a:
for i in range(a.shape[0]):
    for j in range(a.shape[1]):
        a[i,j] = (i+1)*(j+1)*(j+2)
        print 'a[%d,%d]=%g ' % (i,j,a[i,j]),
    print  # newline after each row

print 'a.shape = (2,3); a=', a 
# turn a into a vector of length 6 again
a.shape = (size(a),)   # size(a) returns the total no of elements
print 'a.shape = (size(a),); a=', a 

# mathematical functions:
b = 3*a - 1
b = clip(b, 0.1, 1.0E+20)  # throw away entries < 0.1
c = cos(b)     # take the cosine of all entries in b
print 'b = 3*a - 1; b = clip(b, 0.1, 1.0E+20); c = cos(b)', b, c
# these functions are available:
c = sin(b)   
c = arcsin(c)
c = sinh(b)
# same functions for the cos and tan families
c = b**2.5  # raise all entries to the power of 2.5
c = log(b)
c = exp(b)
c = sqrt(b)

# "in-place" opertions:
print 'in-place operations:'
multiply(a, 3.0, a) # multiply a's entries by 3
print 'multiply(a, 3.0, a); a=', a
subtract(a, 1.0, a) # subtract 1 from a's entries
print 'subtract(a, 1.0, a); a=', a
divide  (a, 3.0, a) # divide a's entries by 3
print 'divide  (a, 3.0, a); a=', a 
add     (a, 1.0, a) # add 1 to a's entries
print 'add     (a, 1.0, a); a=', a
power   (a, 2.0, a) # square all entries
print 'power   (a, 2.0, a); a=', a 
# in-place arithmentic operators:
a *= 3.0
print 'a *= 3.0; a=', a
a -= 1.0   
print 'a -= 1.0; a=', a
a /= 3.0   
print 'a /= 3.0; a=', a
a += 1.0   
print 'a += 1.0; a=', a
a **= 2.0  
print 'a **= 2.0; a=', a

a = array([0, 1.2, 4, -9.1, 5, 8])
a.shape = (2,3) # turn a into a 2x3 matrix

# indexing as for Python lists:
a[2:4] = -1      # set a[2] and a[3] to -1
a[-1] = a[0]     # set last element equal to first one
print 'a[2:4] = -1; a[-1] = a[0]; a=', a 

# multi-dimensional indexing:
a.shape = (3,2)
print 'a.shape = (3,2); a[:,0]',
print a[:,0]     # print first column
print 'a[:,1::2]',
print a[:,1::2]  # print second column with stride 2

b = transpose(a)
print 'transpose(a): ', b

# file reading and writing of NumPy arrays:
a = sequence(min=1, max=20, inc=1, type=Float) # 1-20
a.shape = (2,10)

# ASCII format:
file = open('tmp.dat', 'w')
file.write('Here is an array a:\n')
file.write(repr(a))
# array2string has many options for controlling the
# output of an array as a string, but repr() gives a format
# that can be converted back to an array by eval()
file.close()

# load the array from file into b:
file = open('tmp.dat', 'r')
file.readline()  # load first comment line
b = eval(file.read())
file.close()
# b is a perfect copy of a:
if not allclose(a, b, atol=1.0E-12, rtol=1.0E-12):
    print 'Ooops: b or c are not identical to a; bug in eval(read)...'
else:
    print 'eval(file.read()) works'
    
# binary storage:
a1 = a
a2 = a + 3

import cPickle
file = open('tmp.dat', 'wb')
file.write('This is the array a1:\n')
cPickle.dump(a1, file)
file.write('Here is another array a2:\n')
cPickle.dump(a2, file)
file.close()

file = open('tmp.dat', 'rb')
file.readline()  # swallow the initial comment line
b1 = cPickle.load(file)
file.readline()  # swallow next comment line
b2 = cPickle.load(file)
file.close()
print 'read from binary (pickled) file: b1=', b1[:2,:2], 'b2=', b2[:2,:2]

# pickling NumPy arrays: see also the module scitools.NumPyDB

# binary format using tostring/fromstring:
file = open('tmp3.dat', 'wb')
a_binary = a.tostring()
# store first length (in bytes)
file.write('%d\n%s\n' % (len(a_binary), str(a.shape)))
file.write(a_binary)
file.close()

file = open('tmp3.dat', 'rb')
# load binary data into b:
nbytes = int(file.readline())  # or eval(file.readline())
b_shape = eval(file.readline())
b = fromstring(file.read(nbytes), Float)
b.shape = b_shape
file.close()
print 'read from binary file: b=', b

print '\n\n--------- random numbers -------------\n'
n = 10000  # no of random samples

# native Python support for random numbers:
import random
random.seed() # set seed based on current time
random.seed(2198)  # control the seed
# uniform and random are inherited from whrandom:
print 'random number on (0,1):',         random.random()
print 'unform random number on (-1,1):', random.uniform(-1,1)
print 'N(0,1) uniform random number:',   random.gauss(0,1)

RandomArray.seed(1928)
print 'mean of %d random uniform random numbers:' % n
u = RandomArray.random(n)  # uniform numbers on (0,1)
print 'on (0,1):', sum(u)/n, '(should be 0.5)'
u = RandomArray.uniform(-1,1,n) # uniform numbers on (-1,1)
print 'on (-1,1):', sum(u)/n, '(should be 0)'

# normally distributed numbers:
mean = 0.0; stdev = 1.0
u = RandomArray.normal(mean, stdev, n)
m = sum(u)/n  # empirical mean
s = sqrt(sum((u - m)**2)/(n-1))  # empirical st.dev.
print 'generated %d N(0,1) samples with\nmean %g '\
      'and st.dev. %g using RandomArray.normal' % (n, m, s)

less_than = u < 1.5
p = sum(less_than)
prob = p/float(n)
print 'probability N(0,1) < 1.5: %.2f' % prob

print '\n\n--------- linear algebra -------------\n'

# import LinearAlgebra (or LinearAlgebra2 as LinearAlgebra or linalg
# as LinearAlgebra) is done transparently by NumPy

n = 4
A = zeros(n*n, Float); A.shape = (n,n)
x = zeros(n, Float)
b = zeros(n, Float)

for i in range(n):
    x[i] = i/2.0       # some prescribed solution
    for j in range(n):
        A[i,j] = 2.0 + float(i+1)/float(j+i+1)
b = dot(A,x)  # adjust rhs to fit x

# solve linear system A*y=b:
y = LinearAlgebra.solve_linear_equations(A, b)

# compare exact x with the y we computed:
if sum(abs(x - y)) < 1.0E-12:  print 'correct solution'
else:                          print 'wrong solution',x,y
# alternative:
if allclose(x, y, atol=1.0E-12, rtol=1.0E-12):
    print 'correct solution'
else:
    print 'wrong solution', x, y

# some other operations:
c = innerproduct(x,y)
Ai = LinearAlgebra.inverse(A)
d = LinearAlgebra.determinant(A)
print 'LinearAlgebra.determinant(A) =', d

# test: A times A inverse is the identity matrix:
Inv = LinearAlgebra.inverse # abbreviation
n = len(A)                  # no of rows in A
print 'B is the inverse of A, check ||A*B-I|| = 0, result:',
B = Inv(A)
R = dot(A, B) - identity(n)
R_norm = sqrt(innerproduct(R.flat, R.flat))
print R_norm

d = LinearAlgebra.determinant(A)
print 'det(A)=%g' % d

# eigenvalues only:
A_eigenvalues = LinearAlgebra.eigenvalues(A)

# eigenvalues and eigenvectors:
A_eigenvalues, A_eigenvectors = LinearAlgebra.eigenvectors(A)  

for e, v in zip(A_eigenvalues, A_eigenvectors):
    print 'eigenvalue %g has corresponding vector\n%s' % (e, v)

# the * operator is perhaps not what we expect from mathematics:
print 'A:', A
print 'x:', x
print 'b:', b
print 'A*x =', A*x
print 'b*x =', b*x
print 'dot(A,x) =', dot(A,x)
print 'dot(b,x) =', dot(b,x)


# Some Matlab-like functions:
import MLab
# Note: MLab.py is a good example on using NumPy!



# vectorization:
def somefunc(x):
    """Scalar function."""
    if x < 0:
        return 0
    else:
        return sin(x)

# auto vectorization through numpy.vectorize:
import numpy
somefuncv = numpy.vectorize(somefunc, otypes='d')
somefuncv.__name__ = "vectorize(somefunc)"

def somefunc_NumPy(x):
    r = x.copy()
    for i in xrange(size(x)):
        if x.flat[i] < 0:   # x.flat views x as one-dimensional
            r[i] = 0.0
        else:
            r[i] = sin(x[i])
    r.shape = x.shape
    return r

def somefunc_NumPy2(x):
    """Vectorized version of somefunc."""
    r1 = zeros(len(x), Float)
    r2 = sin(x)
    return where(x < 0, r1, r2)

def somefunc_NumPy2b(x):
    """Vectorized version of somefunc."""
    return where(x < 0, 0.0, sin(x))

def somefunc_NumPy3(x):
    b = (x > zeros(len(x), Float))
    return sin(x)*b

def somefunc_NumPy_log(x):
    r = zeros(len(x), Float)
    for i in range(len(x)):
        if x[i] <= 0:
            r[i] = 0.0
        else:
            r[i] = log(x[i])
    return r

def somefunc_NumPy_logv(x):
    x_pos = where(x > 0, x, 1)
    r1 = log(x_pos)
    r = where(x < 0, 0.0, r1)
    return r

somefunc_list = [somefuncv, somefunc_NumPy, somefunc_NumPy2,
                 somefunc_NumPy2b, somefunc_NumPy3,
                 somefunc_NumPy_log, somefunc_NumPy_logv]

# check correctness:
x = sequence(-2, 2, 2)
print '\nsomefunc_* functions applied to', x
for f in somefunc_list:
    print f(x)

n = 5000000
#n = 500000
x = linspace(0, 2, n+1)
print '\nperforming some timings of "somefunc*" implementations...'
from scitools.EfficiencyTable import EfficiencyTable as ET
from scitools.misc import timer
e = ET('vectorization of "somefunc" functions with an if test, n=%d' % n)
for f in somefunc_list[:-2]:  # skip the last two log functions
    t = timer(f, (x,), repetitions=1)
    e.add(f.__name__, t)
print e
print 'end of', sys.argv[0]
