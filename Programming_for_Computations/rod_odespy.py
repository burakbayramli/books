"""Temperature evolution in a rod, computed by explicit odespy solvers."""

from numpy import linspace, zeros, linspace, array
import matplotlib.pyplot as plt
import time

def rhs(u, t):
    N = len(u) - 1
    rhs = zeros(N+1)
    rhs[0] = dsdt(t)
    for i in range(1, N):
        rhs[i] = (beta/dx**2)*(u[i+1] - 2*u[i] + u[i-1]) + \
                 f(x[i], t)
    rhs[N] = (beta/dx**2)*(2*u[i-1] + 2*dx*dudx(t) -
                           2*u[i]) + f(x[N], t)
    return rhs

def dudx(t):
    return 0

def s(t):
    return 423

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
U_0[1:] = 283
dt = dx**2/(2*beta)
print 'stability limit:', dt
dt *= 100

import odespy
solver = odespy.RKFehlberg(rhs, rtol=1E-6, atol=1E-8)
solver.set_initial_condition(U_0)
T = 1.2
N_t = int(round(T/float(dt)))
time_points = linspace(0, T, N_t+1)
u, t = solver.solve(time_points)

# Check how many time steps required by adaptive vs
# fixed-step methods
if hasattr(solver, 't_all'):
    print '# time steps:', len(solver.t_all)
    plt.figure()
    plt.plot(array(solver.t_all[1:]) - array(solver.t_all[:-1]))
    plt.title('Evolution of the time step in %s' % solver.__class__.__name__)
    plt.savefig('tmp.png'); plt.savefig('tmp.pdf')
    plt.show()
else:
    print '# time steps:', len(t)

# Make movie
import os
os.system('rm tmp_*.png')
plt.figure()
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
    if i % 5 == 0: # plot every 5 steps
        plt.savefig('tmp_%04d.png' % counter)
        counter += 1
    #time.sleep(0.2)
