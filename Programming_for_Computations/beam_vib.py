from numpy import *
from matplotlib.pyplot import *

def f(beta):
    return cosh(beta)*cos(beta) + 1

def damped(beta):
    """Damp the amplitude of f. It grows like cosh, i.e. exp."""
    return exp(-beta)*f(beta)

def plot_f():
    beta = linspace(0, 20, 501)
    #y = f(x)
    y = damped(beta)
    plot(beta, y, 'r', [beta[0], beta[-1]], [0, 0], 'b--')
    grid('on')
    xlabel(r'$\beta$')
    ylabel(r'$e^{-\beta}(\cosh\beta\cos\beta +1)$')
    savefig('tmp1.png'); savefig('tmp1.pdf')
    show()

plot_f()

from nonlinear_solvers import bisection
# Set up suitable intervals
intervals = [[1, 3], [4, 6], [7, 9]]
betas = []  # roots
for beta_L, beta_R in intervals:
    beta, it = bisection(f, beta_L, beta_R, eps=1E-6)
    betas.append(beta)
    print f(beta)
print betas

# Find corresponding frequencies

def omega(beta, rho, A, E, I):
    return sqrt(beta**4/(rho*A/(E*I)))

rho = 7850  # kg/m^3
E = 1.0E+11 # Pa
b = 0.025   # m
h = 0.008   # m
A = b*h
I = b*h**3/12

for beta in betas:
    print omega(beta, rho, A, E, I)
