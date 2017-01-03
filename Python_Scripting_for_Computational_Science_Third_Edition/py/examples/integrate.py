#!/usr/bin/env python
"""Class hierarchy for numerical integration."""

class Integrate:
    def __init__(self):
        self.setup()

    def setup(self):
        # to be overridden in subclasses:
        self.weights = None
        self.points = None 

    def eval(self, f):
        sum = 0.0
        for i in range(len(self.points)):
            sum += self.weights[i]*f(self.points[i])
        return sum

class Trapezoidal(Integrate):
    def setup(self):
        self.points = (-1, 1)
        self.weights = (1, 1)

class Simpson(Integrate):
    def setup(self):
        self.points = (-1, 0, 1)
        self.weights = (1/3.0, 4/3.0, 1/3.0)

class GaussLegendre2(Integrate):
    def setup(self):
        p = 1/math.sqrt(3)
        self.points = (-p, p)
        self.weights = (1, 1)



class TransFunc:
    def __init__(self, f, h):
        self.f = f;  self.h = h

    def coor_mapping(self, xi):
        """Map local xi in (-1,1) in interval j to global x."""
        return (self.j-0.5)*self.h + 0.5*self.h*xi

    def __call__(self, xi):
        x = self.coor_mapping(xi)
        return self.f(x)

def integrate(integrator, a, b, f, n):
    # integrator is an instance of a subclass of Integrator
    sum = 0.0
    h = (b-a)/float(n)
    g = TransFunc(f, h)
    for j in range(1, n+1):
        g.j = j
        sum += integrator.eval(g)
    return 0.5*h*sum

def f(x):
    print 'f(%g)=%g' % (x, 2*x)
    return 2*x

if __name__ == '__main__':
    import sys, math
    try:
        classname = sys.argv[1]
    except:
        print 'Usage: %s Simpson|Trapezoidal|GaussLegendre2' % sys.argv[0]
        sys.exit(1)
    s = eval(classname + '()')
    v = s.eval(lambda x: 2*x)
    print 'integral of f(x)=2*x from -1 to 1 (=0):', v
    
    v = integrate(s, 0, 2, f, 2)
    print 'integral of f(x)=2*x from 0 to 2 (=4):', v
