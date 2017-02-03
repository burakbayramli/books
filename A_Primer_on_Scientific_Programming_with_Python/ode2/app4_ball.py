"""Solve an ODE system for the trajectory of a ball."""

import ODESolver
from scitools.std import *

def f(u, t):
    x, vx, y, vy = u
    g = 9.81
    return [vx, 0, vy, -g]

v0 = 5
theta = 80*pi/180
U0 = [0, v0*cos(theta), 0, v0*sin(theta)]
T = 1.2; dt = 0.01; n = int(round(T/dt))
method = ODESolver.ForwardEuler(f)
method.set_initial_condition(U0)

def terminate(u, t, step_no):
    return False if u[step_no,2] >= 0 else True
    
u, t = method.solve(linspace(0, T, n+1), terminate)
x_values = u[:method.k+2,0]  # or array([x for x, vx, y, vy in u])
y_values = u[:method.k+2,2]  # or array([y for x, vx, y, vy in u])

def exact(x):
    g = 9.81
    y0 = U0[2]  # get y0 from the initial values
    return x*tan(theta) - g*x**2/(2*v0**2)*1/(cos(theta))**2 + y0

plot(x_values, y_values, 'r',
     x_values, exact(x_values), 'b',
     legend=('numerical', 'exact'),
     title='dt=%g' % dt,
     savefig='tmp_ball.eps')
