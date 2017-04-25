"""Temperature evolution in a rod, computed by a ForwardEuler method."""
# As rod_FE.py, but here physical parameters are set for the
# scaled problem.

from numpy import linspace, zeros, linspace
import time

def rhs(u, t):
    N = len(u) - 1
    rhs = zeros(N+1)
    rhs[0] = dsdt(t)
    for i in range(1, N):
        rhs[i] = (beta/dx**2)*(u[i+1] - 2*u[i] + u[i-1]) + \
                 f(x[i], t)
    i = N
    rhs[i] = (beta/dx**2)*(2*u[i-1] + 2*dx*dudx(t) -
                           2*u[i]) + f(x[N], t)
    return rhs

def dudx(t):
    return 0

def s(t):
    return 1

def dsdt(t):
    return 0

def f(x, t):
    return 0


L = 1
beta = 1
N = 40
x = linspace(0, L, N+1)
dx = x[1] - x[0]
u = zeros(N+1)

U_0 = zeros(N+1)
U_0[0] = s(0)
U_0[1:] = 0
dt = dx**2/(2*beta)
print 'stability limit:', dt

t0 = time.clock()
from ode_system_FE import ode_FE
u, t = ode_FE(rhs, U_0, dt, T=1.2)
t1 = time.clock()
print 'CPU time: %.1fs' % (t1 - t0)

# Make movie
import os
os.system('rm tmp_*.png')
import matplotlib.pyplot as plt
plt.ion()
y = u[0,:]
lines = plt.plot(x, y)
plt.axis([x[0], x[-1], -0.1, 1.2*s(0)])
plt.xlabel('x')
plt.ylabel('u(x,t)')
counter = 0
# Plot each of the first 100 frames, then increase speed by 10x
change_speed = 100
for i in range(0, u.shape[0]):
    print t[i]
    plot = True if i <= change_speed else i % 10 == 0
    lines[0].set_ydata(u[i,:])
    if i > change_speed:
        plt.legend(['t=%.3f 10x' % t[i]])
    else:
        plt.legend(['t=%.3f' % t[i]])
    plt.draw()
    if plot:
        plt.savefig('tmp_%04d.png' % counter)
        counter += 1
    #time.sleep(0.2)
