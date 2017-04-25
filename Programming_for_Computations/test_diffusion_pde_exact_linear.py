"""Verify the implementation of the diffusion equation."""

from ode_system_FE import ode_FE
from numpy import linspace, zeros, abs

def rhs(u, t):
    N = len(u) - 1
    rhs = zeros(N+1)
    rhs[0] = dsdt(t)
    for i in range(1, N):
        rhs[i] = (beta/dx**2)*(u[i+1] - 2*u[i] + u[i-1]) + \
                 f(x[i], t)
    rhs[N] = (beta/dx**2)*(2*u[N-1] + 2*dx*dudx(t) -
                           2*u[N]) + f(x[N], t)
    return rhs

def u_exact(x, t):
    return (3*t + 2)*(x - L)

def dudx(t):
    return (3*t + 2)

def s(t):
    return u_exact(0, t)

def dsdt(t):
    return 3*(-L)

def f(x, t):
    return 3*(x-L)


def verify_sympy_ForwardEuler():
    import sympy as sp
    beta, x, t, dx, dt, L = sp.symbols('beta x t dx dt L')
    u = lambda x, t: (3*t + 2)*(x - L)**2
    f = lambda x, t, beta, L: 3*(x-L)**2 - (3*t + 2)*2*beta
    s = lambda t: (3*t + 2)*L**2
    N = 4
    rhs = [None]*(N+1)
    rhs[0] = sp.diff(s(t), t)
    for i in range(1, N):
        rhs[i] = (beta/dx**2)*(u(x+dx,t) - 2*u(x,t) + u(x-dx,t)) + \
                 f(x, t, beta, L)
    rhs[N] = (beta/dx**2)*(u(x-dx,t) + 2*dx*(3*t+2) -
                           2*u(x,t) + u(x-dx,t)) + f(x, t, beta, L)
    for i in range(len(rhs)):
        rhs[i] = sp.simplify(sp.expand(rhs[i])).subs(x, i*dx)
        print rhs[i]
        lhs = (u(x, t+dt) - u(x,t))/dt  # Forward Euler difference
        lhs = sp.simplify(sp.expand(lhs.subs(x, i*dx)))
        print lhs
        print sp.simplify(lhs - rhs[i])
        print '---'

def test_diffusion_exact_linear():
    global beta, dx, L, x  # needed in rhs
    L = 1.5
    beta = 0.5
    N = 4
    x = linspace(0, L, N+1)
    dx = x[1] - x[0]
    u = zeros(N+1)

    U_0 = zeros(N+1)
    U_0[0] = s(0)
    U_0[1:] = u_exact(x[1:], 0)
    dt = 0.1
    print dt

    u, t = ode_FE(rhs, U_0, dt, T=1.2)

    tol = 1E-12
    for i in range(0, u.shape[0]):
        diff = abs(u_exact(x, t[i]) - u[i,:]).max()
        assert diff < tol, 'diff=%.16g' % diff
        print 'diff=%g at t=%g' % (diff, t[i])


if __name__ == '__main__':
    test_diffusion_exact_linear()
    verify_sympy_ForwardEuler()
