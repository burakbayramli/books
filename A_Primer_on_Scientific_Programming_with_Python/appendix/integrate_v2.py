def integrate(f, a, b, n):
    h = float(b-a)/n
    s = 0
    for i in range(1, n+1):
        s += f(a + (i-0.5)*h)
    return s*h

def f(x):
    return asin(x)

def g(x):
    return 1

# Test/application part
import sys
n = int(sys.argv[1])
I = integrate(g, 0, 10,  n) 
print "Integral of g equals %g" % I
from math import *
I = integrate(f, 0, 1,  n)      
I_exact = 1*asin(1) - sqrt(1 - 1**2) - 1
print "Integral of f equals %g (exact value is %g)" % \
  (I, I_exact)
