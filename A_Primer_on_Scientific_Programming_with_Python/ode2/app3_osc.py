"""
Equation:

       m*u'' + beta*u' + k*u = m*w''(t) + m*g

written as a 2x2 first-order ODE system and solved
by classes in the ODESolver hierarchy of methods.
"""

class OscSystem:
    def __init__(self, m, beta, k, g, w):
        self.m, self.beta, self.k, self.g, self.w = \
                float(m), float(beta), float(k), float(g), w

    def __call__(self, u, t):
        u0, u1 = u
        m, beta, k, g, w = \
           self.m, self.beta, self.k, self.g, self.w
        # Use a finite difference for w''(t)
        h = 1E-5
        ddw = (w(t+h) - 2*w(t) + w(t-h))/(h**2)
        f = [u1, ddw  + g - beta/m*u1 - k/m*u0]
        return f

# Test case: u = cos(t)
import ODESolver
from scitools.std import *
f = OscSystem(1.0, 0.0, 1.0, 0.0, lambda t: 0)
u_init = [1, 0]    # initial condition
nperiods = 3.5     # no of oscillation periods
T = 2*pi*nperiods
for method_class in ODESolver.ForwardEuler, ODESolver.RungeKutta4:
    if method_class == ODESolver.ForwardEuler:
        npoints_per_period = 200
    elif method_class == ODESolver.RungeKutta4:
        npoints_per_period = 20
    n = npoints_per_period*nperiods
    t_points = linspace(0, T, n+1)
    method = method_class(f)
    method.set_initial_condition(u_init)
    u, t = method.solve(t_points)

    # u is an array of [u0,u1] pairs for each time level,
    # get the u0 values from u for plotting
    u0_values = u[:, 0]
    u1_values = u[:, 1]
    u0_exact = cos(t)
    u1_exact = -sin(t)
    figure()
    alg = method_class.__name__  # (class) name of algorithm
    plot(t, u0_values, 'r-',
         t, u0_exact, 'b-',
         legend=('numerical', 'exact'),
         title='Oscillating system; position - %s' % alg,
         savefig='tmp_oscsystem_pos_%s.eps' % alg)
    figure()
    plot(t, u1_values, 'r-',
         t, u1_exact, 'b-',
         legend=('numerical', 'exact'),
         title='Oscillating system; velocity - %s' % alg,
         savefig='tmp_oscsystem_vel_%s.eps' % alg)
