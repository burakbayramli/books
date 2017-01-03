#!/usr/bin/env python
from scitools.numpyutils import *

f1 = wrap2callable(2.0)
print "constant:", f1(0.5), f1.__class__.__name__

f2 = wrap2callable('1+2*x')
print "string formula of x:", f2(0.5), f2.__class__.__name__

f3 = wrap2callable('1+2*t', independent_variables='t')
print "string formula of t:", f3(0.5), f3.__class__.__name__

f4 = wrap2callable('a+b*t', independent_variables='t', a=1, b=2)
print "string formula with parameters:", f4(0.5), f4.__class__.__name__

x = seq(0,1,0.5); y=1+2*x
f5 = wrap2callable((x,y))
print "interpolate discrete data (at a grid point):", f5(0.5)
print "interpolate again (not at a grid point):", f5(0.51), 1+2*0.51
print "interpolation object type:", f5.__class__.__name__

def myfunc(x):
    return 1+2*x
f6 = wrap2callable(myfunc)
print "user-defined function", f6(0.5), f6.__class__.__name__

f7 = wrap2callable(lambda x: 1+2*x)
print "user-defined anynomous lambda function:", f7(0.5), \
      f7.__class__.__name__

class MyClass:
    """Representation of a function f(x; a, b) =a + b*x"""
    def __init__(self, a=1, b=1):
        self.a = a;  self.b = b
    def __call__(self, x):
        return self.a + self.b*x
myclass = MyClass(a=1, b=2)
f8 = wrap2callable(myclass)
print "user-defined class with __call__ method:", f8(0.5), \
      f8.__class__.__name__

# three-dimensional functions:
f9 = wrap2callable('1+2*x+3*y+4*z', independent_variables=('x','y','z'))
print "3D string formula:", f9(0.5,1/3.,0.25), f9.__class__.__name__
# we know that f9 is a StringFunction instance:
print "This is the formula:", str(f9)
print "Construction of instance:", repr(f9)
# reconstruct:
f9b = eval(repr(f9))
if repr(f9b) != repr(f9):  print 'Bug in StringFunction.__repr__'

y = seq(0,1,0.5)
z = seq(-1,0.5,0.1)
# for a three-dimensional grid use
xv = x[:,newaxis,newaxis]
yv = y[newaxis,:,newaxis]
zv = z[newaxis,newaxis,:]
def myfunc3(x, y, z):  return 1+2*x+3*y+4*z
values = myfunc3(xv, yv, zv)
f10 = wrap2callable((x, y, z, values))
print "interpolate 3D discrete data:", f10(0.5, 1/3., 0.25), \
      f10.__class__.__name__
