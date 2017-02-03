from math import *

class FuncWithDerivatives:
    def __init__(self, h=1.0E-9):
        self.h = h  # spacing for numerical derivatives
    def __call__(self, x):
        raise NotImplementedError('___call__ missing in class %s' % 
                                  self.__class__.__name__)
    def df(self, x):
        """Return the 1st derivative of self.f."""
        # Compute first derivative by a finite difference
        h = self.h
        return (self(x+h) - self(x-h))/(2.0*h)
    
    def ddf(self, x):
        """Return the 2nd derivative of self.f."""
        # Compute second derivative by a finite difference
        h = self.h
        return (self(x+h) - 2*self(x) + self(x-h))/(float(h)**2)

class MyFunc(FuncWithDerivatives):
    def __init__(self, a):
        self.a = a
    def __call__(self, x):
        return cos(self.a*x) + x**3
    def df(self, x):
        a = self.a
        return -a*sin(a*x) + 3*x**2
    def ddf(self, x):
        a = self.a
        return -a*a*cos(a*x) + 6*x


f = MyFunc(4)
f(pi)
f.df(pi)
f.ddf(pi)

class MyComplicatedFunc(FuncWithDerivatives):
    def __init__(self, p, q, r, h=1.0E-9):
        FuncWithDerivatives.__init__(self, h)
        self.p, self.q, self.r = p, q, r
    def __call__(self, x):
        return log(abs(self.p*tanh(self.q*x*cos(self.r*x))))


f = MyComplicatedFunc(1, 1, 1)
x = pi/2
f(x)
f.df(x)
f.ddf(x)


from Diff import *
mycos = Central4(sin)
# Compute sin'(pi):
mycos(pi)
cos(pi)  # compare with exact result

from shapes import *
Shape.set_coordinate_system(xmin=0, xmax=10, ymin=0, ymax=10)
l1 = Line(start=(0,0), stop=(1,1))  # define line
l1.draw()        # make all plot data
r1 = Rectangle(lower_left_corner=(0,1), width=3, height=5)
r1.draw()
Circle(center=(5,7), radius=1).draw()
Wheel(center=(6,2), radius=2, inner_radius=0.5, nlines=8).draw()
Shape.hardcopy()


class SuperClass:
    def __init__(self, p, q):
        self.p, self.q = p, q
    def where(self):
        print 'In superclass', self.__class__.__name__
    def compute(self, x):
        self.where()
        return self.p*x + self.q


class SubClass(SuperClass):
    def __init__(self, p, q, a):
        SuperClass.__init__(self, p, q)
        self.a = a
    def where(self):
        print 'In subclass', self.__class__.__name__
    def compute(self, x):
        self.where()
        return SuperClass.compute(self, x) + self.a*x**2

super = SuperClass(1, 2)
sub = SubClass(1, 2, 3)
v1 = super.compute(0)
v2 = sub.compute(0)
