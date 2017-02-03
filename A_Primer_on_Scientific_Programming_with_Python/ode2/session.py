import numpy as np

def ForwardEuler(f_user, dt, U0, T):
    """Integrate u'=f(u,t), u(0)=u0, in steps of dt until t=T."""
    n = int(round(T/dt))
    t = np.zeros(n+1)
    f = lambda u, t: np.asarray(f_user(u, t))
    if isinstance(U0, (float,int)):  # scalar ODE
        u = np.zeros(n+1)  
    else:                            # system of ODEs
        U0 = np.asarray(U0)
        neq = U0.size
        u = np.zeros((n+1,neq))  
    u[0] = U0
    t[0] = 0
    for k in range(n):
        t[k+1] = t[k] + dt
        u[k+1] = u[k] + dt*f(u[k], t[k])
    return u, t

def f(u, t):
    return [u[1], -u[0]]

U0 = [0, 1]

from scitools.std import *
dt = pi/20
T = 4*pi
u, t = ForwardEuler(f, dt, U0, T)
u1 = u[:,0]
plot(t, u1)
legend('ForwardEuler function')

import collections

class ForwardEuler:
    """
    Class for solving a scalar of vector ODE,

      du/dt = f(u, t)

    by the ForwardEuler method.
    
    Class attributes:
    t: array of time values
    u: array of solution values (at time points t)
    k: step number of the most recently computed solution
    f: callable object implementing f(u, t)
    """
    def __init__(self, f):
        if not callable(f):
            raise TypeError('f is %s, not a function' % type(f))
        self.f = lambda u, t: np.asarray(f(u, t))

    def set_initial_condition(self, U0):
        if isinstance(U0, (float,int)):  # scalar ODE
            self.neq = 1
        else:                            # system of ODEs
            U0 = np.asarray(U0)
            self.neq= U0.size
        self.U0 = U0

    def solve(self, time_points):
        """Compute u for t values in time_points list."""
        self.t = np.asarray(time_points)
        n = self.t.size
        if self.neq == 1:  # scalar ODEs
            self.u = np.zeros(n)
        else:              # systems of ODEs
            self.u = np.zeros((n,self.neq))

        # Assume self.t[0] corresponds to self.U0
        self.u[0] = self.U0

        # Time loop
        for k in range(n-1):
            self.k = k
            self.u[k+1] = self.advance()
        return self.u, self.t

    def advance(self):
        """Advance the solution one time step."""
        u, f, k, t = self.u, self.f, self.k, self.t
        dt = t[k+1] - t[k]
        unew = u[k] + dt*f(u[k], t[k])
        return unew

method = ForwardEuler(f)
method.set_initial_condition(U0)
t = np.linspace(0, T, t.size)
u, t = method.solve(t)
u1 = u[:,0]
figure()
hold('on')
plot(t, u1)
legend('Class ForwardEuler')
