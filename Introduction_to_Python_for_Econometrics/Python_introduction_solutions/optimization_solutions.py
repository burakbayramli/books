"""Solutions for 'Optimization' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('optimization_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function
from numpy import dot, pi, log, mean, array, zeros, var, finfo, sqrt
from numpy.linalg import lstsq
from numpy.random import randn
from scipy.optimize import fmin_bfgs, fmin, fmin_slsqp
# <demo> --- stop ---
# Negative Log-likellihood
def normal_loglik(mu,x):
    n = x.size
    epe = dot(x - mu,x - mu)
    sigma2 = epe/n
    return 0.5 * (n * log(2*pi) + n * log(sigma2) + epe/sigma2)

# <demo> --- stop ---
# Negative Log-likellihood 2    
def normal_loglik2(p, x):
    mu = p[0]
    sigma2 = p[1]
    if sigma2<0:
        sigma2 = finfo(float).tiny
    epe = dot(x - mu,x - mu)
    n = x.size
    return 0.5 * (n * log(2*pi) + n * log(sigma2) + epe/sigma2)

# <demo> --- stop ---
# Negative Log-likellihood 3
def normal_loglik3(p, x):
    mu = p[0]
    sigma2 = p[1]**2
    epe = dot(x - mu,x - mu)
    n = x.size
    return 0.5 * (n * log(2*pi) + n * log(sigma2) + epe/sigma2)

# <demo> --- stop ---
# Negative Log-likellihood 4
def ols_loglik(b, y, X):
    bv = b.view()
    K = X.shape[1]
    bv.shape=K,1
    e = y-dot(X,bv)
    n = e.size
    epe = dot(e.T,e)
    
    sigma2 = epe/n
    return 0.5 * (n * log(2*pi) + n * log(sigma2) + epe/sigma2)
    
# <demo> --- stop ---
# Constraint function
def constraint(p, x):
    return array([1,p[1]-.0001])

# <demo> --- stop ---
# Exercise 1    
x = randn(1000) + 2
mu0 = mean(x) + 1
args = (x,)

mu = fmin_bfgs(normal_loglik, mu0, args = args)
print("mu:")
print(mu)
print("mean(x):")
print(mean(x))

mu = fmin(normal_loglik, mu0, args = args)
print("mu:")
print(mu)
# <demo> --- stop ---
# Exercise 2
p0 = zeros(2)
p0[0] = mean(x) + 1
p0[1] = var(x) + 2

print("normal_loglik2(p0,x):")
print(normal_loglik2(p0,x))

p = fmin_slsqp(normal_loglik2, p0, args = args, bounds = [(-100,100),(0.000001,100)])
print("array([mean(x),var(x)]):")
print(array([mean(x),var(x)]))
print("p:")
print(p)

p = fmin_slsqp(normal_loglik2, p0, args = args, f_ieqcons = constraint)
print("p:")
print(p)
# <demo> --- stop ---
# Exercise 3
p0[1] = sqrt(p0[1])
p = fmin_bfgs(normal_loglik3, p0, args=args)
p[1] = p[1]**2
print("p:")
print(p)

# <demo> --- stop ---
# Exercise 4
b = array([[1.0],[2]])
X = randn(1000,2)
y = dot(X,b) + randn(1000,1)
b0 = lstsq(X,y)
b0 = b0[0]
print("b0:")
print(b0)
args = (y,X)
print("ols_loglik(b0.ravel(),y,X):")
print(ols_loglik(b0.ravel(),y,X))
b = fmin_slsqp(ols_loglik,b0.ravel(),args=args)
print("b:")
print(b)
