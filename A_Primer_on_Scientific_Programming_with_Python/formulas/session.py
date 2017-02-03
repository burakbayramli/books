# Complex numbers
u = 2.5 + 3j       # create a complex number
v = 2              # this is an int
w = u+v            # complex + int
w

a = -2
b = 0.5
s = a + b*1j       # create a complex number from two floats
s = complex(a, b)  # alternative creation
s*w                # complex*complex
s/w                # complex/complex

from math import sin
r = sin(w)

from cmath import sin, sinh, cos, exp
r1 = sin(8j)
r1
r2 = sinh(8)
r2
q = 8
exp(1j*q)
cos(q) + 1j*sin(q)*1j

from math import sqrt
sqrt(4)     # float
sqrt(-1)    # illegal
from cmath import sqrt
sqrt(4)     # complex
sqrt(-1)    # complex
from scitools.std import *
sqrt(4)     # float
sqrt(-1)    # complex

# Roots of quadratic polynomials
#from scitools.std import *
a = 1; b = 2; c = 100   # polynomial coefficients
r1 = (b + sqrt(b**2 - 4*a*c))/(2*a)
r2 = (b - sqrt(b**2 - 4*a*c))/(2*a)
r1
r2

a = 1; b = 4; c = 1   # polynomial coefficients
r1 = (b + sqrt(b**2 - 4*a*c))/(2*a)
r2 = (b - sqrt(b**2 - 4*a*c))/(2*a)
r1
r2
