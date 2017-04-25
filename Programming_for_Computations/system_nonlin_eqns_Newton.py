import sys
from numpy import matrix, sqrt

def f(x, y):
    return matrix([[y - x**2, y - 2 + x**4]])

def J(x):
    return matrix([[-2*x, 1], [4*x**3, 1]])

x = matrix([[2.0, 2.0]]).T   # Starting values
f_eval = f(float(x[[0],[0]]), float(x[[1],[0]]))
error_limit = 0.001    
no_iterations = 0
it_limit = 100
while sqrt(f_eval[[0],[0]]**2 + f_eval[[0],[1]]**2) > error_limit and \
                                            no_iterations < it_limit:
    try:
        x = x - J(float(x[[0],[0]])).I*f(float(x[[0],[0]]),\
                                            float(x[[1],[0]])).T
        print x, '\n'
        f_eval = f(float(x[[0],[0]]), float(x[[1],[0]]))
        no_iterations += 1
    except:
        print "Error! - Jacobian not invertible for x = ", x
        sys.exit(1)   # Abort with error
