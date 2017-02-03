def table(f, x, h_values, methods, dfdx=None):
    """
    Write a table of f'(x) computed numerically by
    the methods in the methods list (class names).
    Each row in the table corresponds to a value of
    the discretization parameter h (in h_values).
    If dfdx is not None, dfdx is the exact derivative
    (a function) and the entries in the table are
    the errors in the numerical approximations.
    """
    # Print headline (h and class names for the methods)
    print '      h       ',
    for method in methods:
        print '%-15s' % method.__name__,
    print  # newline
    # Print table
    for h in h_values:
        print '%10.2E' % h,
        for method in methods:
            if dfdx is not None:
                d = method(f, h, dfdx)
                output = d.error(x)
            else:
                d = method(f, h)
                output = d(x)
            print '%15.8E' % output,
        print  # newline

from Diff2 import *

# Example 1: f(x) = x**2

table(f=lambda x: x*x,
      x=0.5,
      h_values=[2**(-k) for k in range(10)],
      methods=[Forward1, Central2, Central4],
      dfdx=lambda x: 2*x)

# Example 2: f(x) = exp(-10*x)

from math import exp
def f1(x):
    return exp(-10*x)

def df1dx(x):
    return -10*exp(-10*x)

table(f=f1,
      x=0,
      h_values=[2**(-k) for k in range(10)],
      methods=[Forward1, Central2, Central4],
      dfdx=df1dx)


        
    
