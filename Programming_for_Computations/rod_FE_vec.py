"""Temperature evolution in a rod, computed by a ForwardEuler method."""

from numpy import linspace, zeros, linspace
import time

def rhs(u, t):
    N = len(u) - 1
    rhs = zeros(N+1)
    rhs[0] = dsdt(t)
    rhs[1:N] = (beta/dx**2)*(u[2:N+1] - 2*u[1:N] + u[0:N-1]) + \
               g(x[1:N], t)
    i = N
    rhs[i] = (beta/dx**2)*(2*u[i-1] + 2*dx*dudx(t) -
                           2*u[i]) + g(x[i], t)
    return rhs

def dudx(t):
    return 0

def s(t):
    return 423

def dsdt(t):
    return 0

def g(x, t):
    return 0


L = 1
beta = 1
N = 100
x = linspace(0, L, N+1)
dx = x[1] - x[0]
u = zeros(N+1)

U_0 = zeros(N+1)
U_0[0] = s(0)
U_0[1:] = 283
dt = dx**2/(2*beta)
print 'stability limit:', dt
#dt = 0.00034375

t0 = time.clock()
from ode_system_FE import ode_FE
u, t = ode_FE(rhs, U_0, dt, T=1.2)
t1 = time.clock()
print 'CPU time: %.1fs' % (t1 - t0)
import sys; sys.exit(0)

# Make movie
import os
os.system('rm tmp_*.png')
import matplotlib.pyplot as plt
plt.ion()
y = u[0,:]
lines = plt.plot(x, y)
plt.axis([x[0], x[-1], 273, 1.2*s(0)])
plt.xlabel('x')
plt.ylabel('u(x,t)')
counter = 0
for i in range(0, u.shape[0]):
    print t[i]
    lines[0].set_ydata(u[i,:])
    plt.legend(['t=%.3f' % t[i]])
    plt.draw()
    if i % 10 == 0: # plot every x steps
        plt.savefig('tmp_%04d.png' % counter)
        counter += 1
    #time.sleep(0.2)
