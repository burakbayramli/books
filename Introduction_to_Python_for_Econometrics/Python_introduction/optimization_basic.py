from __future__ import division
from __future__ import print_function
import scipy.optimize as opt
import sys
from pylab import *
from numpy import *
# End Imports


def optim_target1(x):
    return x**2

def optim_target2(params):
    x, y = params

    return x**2-3*x+3+y*x-3*y+y**2

def optim_target3(params,hyperparams):
    x, y = params
    c1, c2, c3=hyperparams

    return x**2+c1*x+c2+y*x+c3*y+y**2


opt.fmin_bfgs(optim_target1, 2)

def optim_target1_grad(x):
    return 2*x

opt.fmin_bfgs(optim_target1, 2, fprime = optim_target1_grad)

opt.fmin_bfgs(optim_target2, array([1.0,2.0]))

hyperp = array([1.0,2.0,3.0])
opt.fmin_bfgs(optim_target3, array([1.0,2.0]), args=(hyperp,))

def optim_target3_grad(params,hyperparams):
    x, y = params
    c1, c2, c3=hyperparams
    return array([2*x+c1+y,x+c3+2*y])

optimum = opt.fmin_bfgs(optim_target3, array([1.0,2.0]), fprime=optim_target3_grad, args=(hyperp ,))
optimum
optim_target3_grad(optimum, hyperp) # Numerical zero

opt.fmin_cg(optim_target3, array([1.0,2.0]), args=(hyperp ,))

opt.fmin_ncg(optim_target3, array([1.0,2.0]), optim_target3_grad, args=(hyperp,))

def optim_target3_hess(params,hyperparams):
    x, y = params
    c1, c2, c3=hyperparams
    return(array([[2, 1],[1, 2]]))

opt.fmin_ncg(optim_target3, array([1.0,2.0]), optim_target3_grad, \
fhess = optim_target3_hess, args=(hyperp ,))

def tick_loss(quantile, data, alpha):
    e = data - quantile
    return dot((alpha - (e<0)),e)

data = randn(1000)
opt.fmin(tick_loss, 0, args=(data, 0.5))
median(data)

data = randn(1000)
opt.fmin_powell(tick_loss, 0, args=(data, 0.5))

def utility(x, p, alpha):
    # Minimization, not maximization so -1 needed
    return -1.0 * (x[0]**alpha)*(x[1]**(1-alpha))

def utility_constraints(x, p, alpha):
    return array([x[0], x[1], 1 - p[0]*x[0] - p[1]*x[1]])

p = array([1.0,1.0])
alpha = 1.0/3
x0 = array([.4,.4])
opt.fmin_slsqp(utility, x0, f_ieqcons=utility_constraints, args=(p, alpha))

def utility_grad(x, p, alpha):
    grad = zeros(2)
    grad[0] = -1.0 * alpha * (x[0]**(alpha-1))*(x[1]**(1-alpha))
    grad[1] = -1.0 * (1-alpha) * (x[0]**(alpha))*(x[1]**(-alpha))
    return grad

def utility_constraint_grad(x, p, alpha):
    grad = zeros((3,2)) # 3 constraints, 2 variables
    grad[0,0] = 1.0
    grad[0,1] = 0.0
    grad[1,0] = 0.0
    grad[1,1] = 1.0
    grad[2,0] = -p[0]
    grad[2,1] = -p[1]
    return grad

opt.fmin_slsqp(utility, x0, f_ieqcons=utility_constraints, args=(p, alpha), \
fprime = utility_grad, fprime_ieqcons = utility_constraint_grad)

def utility_constraints_alt(x, p, alpha):
    return array([1 - p[0]*x[0] - p[1]*x[1]])

opt.fmin_slsqp(utility, x0, f_ieqcons=utility_constraints_alt, args=(p, alpha), \
bounds = [(0.0,2.0),(0.0,2.0)])

def total_expenditure(x,p,alpha,Ubar):
    return dot(x,p)

def min_utility_constraint(x,p,alpha,Ubar):
    x1,x2 = x
    u=x1**(alpha)*x2**(1-alpha)
    return array([u - Ubar]) # >= constraint, must be array, even if scalar

x0 = array([1.0,1.0])
p = array([1.0,1.0])
alpha = 1.0/3
Ubar = 0.529133683989
opt.fmin_slsqp(total_expenditure, x0, f_ieqcons=min_utility_constraint, \
    args=(p, alpha, Ubar), bounds =[(0.0,2.0),(0.0,2.0)])

def utility_constraints1(x, p, alpha):
    return x[0]
def utility_constraints2(x, p, alpha):
    return x[1]
def utility_constraints3(x, p, alpha):
    return (1 - p[0]*x[0] - p[1]*x[1])

p = array([1.0,1.0])
alpha = 1.0/3
x0 = array([.4,.4])
cons = [utility_constraints1, utility_constraints2, utility_constraints3]
opt.fmin_cobyla(utility, x0, cons, args=(p, alpha), rhoend=1e-7)

def reparam_utility(z,p,alpha,printX = False):
    x = exp(z)/(1+exp(z))
    x[0] = (1.0/p[0]) * x[0]
    x[1] = (1-p[0]*x[0])/p[1] * x[1]
    if printX:
        print(x)
    return -1.0 * (x[0]**alpha)*(x[1]**(1-alpha))

x0 = array([.4,.4])
optX = opt.fmin_bfgs(reparam_utility, x0, args=(p,alpha))
reparam_utility(optX, p, alpha, printX=True)

def optim_target5(x, hyperparams):
    c1,c2,c3 = hyperparams
    return c1*x**2 + c2*x + c3

hyperp = array([1.0, -2.0, 3])
opt.fminbound(optim_target5, -10, 10, args=(hyperp,))
opt.fminbound(optim_target5, -10, 0, args=(hyperp,))

hyperp = array([1.0, -2.0, 3])
opt.golden(optim_target5, args=(hyperp,))
opt.golden(optim_target5, args=(hyperp,), brack=[-10.0,10.0])

opt.brent(optim_target5, args=(hyperp,))

def nlls_objective(beta, y, X):
    b0 = beta[0]
    b1 = beta[1]
    b2 = beta[2]
    return y - b0 - b1 * (X**b2)

X = 10 *rand(1000)
e = randn(1000)
y = 10 + 2 * X**(1.5) + e
beta0 = array([10.0,2.0,1.5])
opt.leastsq(nlls_objective, beta0, args = (y, X))

