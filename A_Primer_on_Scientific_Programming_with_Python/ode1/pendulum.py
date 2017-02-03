#!/usr/bin/env python

def pendulum(T, n, theta0, v0, alpha):
    """Return the motion (theta, v, t) of a pendulum."""
    dt = T/float(n)
    t = linspace(0, T, n+1)
    v = zeros(n+1)
    theta = zeros(n+1)
    v[0] = v0
    theta[0] = theta0
    for k in range(n):
        theta[k+1] = theta[k] + dt*v[k]
        v[k+1] = v[k] - alpha*dt*sin(theta[k+1])
    return theta, v, t

from scitools.std import *
try:
    n = int(sys.argv[1])
    T = eval(sys.argv[2])
    v0 = eval(sys.argv[3])
    theta0 = eval(sys.argv[4])
    alpha = eval(sys.argv[5])
except:
    print "usage:", sys.argv[0], "n T v0 theta0 alpha"
    sys.exit(1)

theta, v, t = pendulum(T, n, theta0, v0)
plot(t, v, xlabel='t', ylabel='velocity')
figure()
plot(t, theta, xlabel='t', ylabel='velocity')

