"""Function implementing the Forward Euler method for scalar ODEs."""

def ForwardEuler(f, U0, T, n):
    """Solve u'=f(u,t), u(0)=U0, with n steps until t=T."""
    import numpy as np
    t = np.zeros(n+1)
    u = np.zeros(n+1)  # u[k] is the solution at time t[k]
    u[0] = U0
    t[0] = 0
    dt = T/float(n)
    for k in range(n):
        t[k+1] = t[k] + dt
        u[k+1] = u[k] + dt*f(u[k], t[k])
    return u, t

# Problem: u'=u
def f(u, t):
    return u

u, t = ForwardEuler(f, U0=1, T=3, n=30)

# Compare numerical solution and exact solution in a plot
from scitools.std import plot, exp
u_exact = exp(t)
plot(t, u, 'r-', t, u_exact, 'b-',
     xlabel='t', ylabel='u', legend=('numerical', 'exact'),
     title="Solution of the ODE u'=u, u(0)=1")

# Verify by hand calculations
u, t = ForwardEuler(f, U0=1, T=0.2, n=2)
print u, [1, 1,1, 1.21]

# Verify by exact numerical solution
def f1(u, t):
    return 0.2 + (u - u_solution_f1(t))**4

def u_solution_f1(t):
    return 0.2*t + 3

u, t = ForwardEuler(f1, U0=3, T=3, n=5)
print 'Numerical:', u
print 'Exact:    ', u_solution_f1(t)

# Accuracy check for decreasing time step (u'=u, u(0)=1)
T = 3; U0 = 1
for dt in 0.5, 0.05, 0.005:
    n = int(round(T/dt))
    u, t = ForwardEuler(f, U0, T, n)
    print 'dt=%.5f, u(%g)=%.6f, error=%g' % \
          (dt, t[-1], u[-1], exp(t[-1]-u[-1]))
    
