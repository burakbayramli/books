"""Verify the implementation of the diffusion equation."""

from ode_system_FE import ode_FE
from numpy import linspace, zeros, linspace, abs

def s(t):
    return u_exact(0, t)

def dsdt(t):
    return 1

def f(x, t):
    return 1 - 2*beta

def rhs(u, t):
    N = len(u) - 1
    rhs = zeros(N+1)
    rhs[0] = dsdt(t)
    for i in range(1, N):
        rhs[i] = (beta/dx**2)*(u[i+1] - 2*u[i] + u[i-1]) + f(x[i], t)
    rhs[N] = (2*beta/dx**2)*(u[i-1] - u[i]) + f(x[N], t)
    return rhs

def u_exact(x, t):
    return t + (x - L)**2

def verify_sympy():
    import sympy as sp
    beta, x, t, dx, dt, L = sp.symbols('beta x t dx dt L')
    u = lambda x, t: 3*t + 2*x
    f = lambda x, t, beta, L: 3
    s = lambda t: u(0, t)
    N = 4
    rhs = [None]*(N+1)
    rhs[0] = sp.diff(s(t), t)
    for i in range(1, N):
        rhs[i] = (beta/dx**2)*(u(x+dx,t) - 2*u(x,t) + u(x-dx,t)) + \
                 f(x, t, beta, L)
    rhs[N] = (beta/dx**2)*(u(x-dx,t) + 2*dx*2 - 2*u(x,t) + u(x-dx,t)) + \
                 f(x, t, beta, L)
    #rhs[N] = (2*beta/dx**2)*(u(x-dx,t) - u(x,t)) + f(x, t, beta, L)
    for i in range(len(rhs)):
        rhs[i] = sp.simplify(sp.expand(rhs[i])).subs(x, i*dx)
        print rhs[i]
        lhs = (u(x, t+dt) - u(x,t))/dt
        lhs = sp.simplify(sp.expand(lhs.subs(x, i*dx)))
        print lhs
        print sp.simplify(lhs - rhs[i])
        print '---'


L = 1.5
beta = 0.5
N = 40
x = linspace(0, L, N+1)
dx = x[1] - x[0]
u = zeros(N+1)

U_0 = zeros(N+1)
U_0[0] = s(0)
U_0[1:] = u_exact(x[1:], 0)
dt = dx**2/(2*beta)
print 'stability limit:', dt

u, t = ode_FE(rhs, U_0, dt, T=1.2)

for i in range(0, u.shape[0]):
    diff = abs(u_exact(x, t[i]) - u[i,:]).max()
    #print u[i,:]
    #print u_exact(x, t[i])
    print 'diff=%g at t=%g' % (diff, t[i])
    print '---'

verify_sympy()
