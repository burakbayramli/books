import numpy as np

class ODESolver:
    """
    Superclass for numerical methods solving scalar and vector ODEs

      du/dt = f(u, t)

    Attributes:
    t: array of time values
    u: array of solution values (at time points t)
    k: step number of the most recently computed solution
    f: callable object implementing f(u, t)
    """
    def __init__(self, f):
        if not callable(f):
            raise TypeError('f is %s, not a function' % type(f))
        # For ODE systems, f will often return a list, but
        # arithmetic operations with f in numerical methods
        # require that f is an array. Let self.f be a function
        # that first calls f(u,t) and then ensures that the
        # result is an array of floats.
        self.f = lambda u, t: np.asarray(f(u, t), float)

    def advance(self):
        """Advance solution one time step."""
        raise NotImplementedError

    def set_initial_condition(self, U0):
        if isinstance(U0, (float,int)):  # scalar ODE
            self.neq = 1
            U0 = float(U0)
        else:                            # system of ODEs
            U0 = np.asarray(U0)          # (assume U0 is sequence)
            self.neq = U0.size
        self.U0 = U0

    def solve(self, time_points, terminate=None):
        """
        Compute solution u for t values in the list/array
        time_points, as long as terminate(u,t,step_no) is False. 
        terminate(u,t,step_no) is a user-given function
        returning True or False. By default, a terminate
        function which always returns False is used.
        """
        if terminate is None:
            terminate = lambda u, t, step_no: False

        if isinstance(time_points, (float,int)):
            raise TypeError('solve: time_points is not a sequence')
        
        self.t = np.asarray(time_points)
        n = self.t.size
        if self.neq == 1:  # scalar ODEs
            self.u = np.zeros(n)
        else:              # systems of ODEs
            self.u = np.zeros((n,self.neq))

        # Assume that self.t[0] corresponds to self.U0
        self.u[0] = self.U0

        # Time loop
        for k in range(n-1):
            self.k = k
            self.u[k+1] = self.advance()
            if terminate(self.u, self.t, self.k+1):
                break  # terminate loop over k
        return self.u, self.t
    

class ForwardEuler(ODESolver):
    def advance(self):
        u, f, k, t = self.u, self.f, self.k, self.t
        dt = t[k+1] - t[k]
        unew = u[k] + dt*f(u[k], t[k])
        return unew

class RungeKutta4(ODESolver):
    def advance(self):
        u, f, k, t = self.u, self.f, self.k, self.t
        dt = t[k+1] - t[k]
        dt2 = dt/2.0
        K1 = dt*f(u[k], t[k])
        K2 = dt*f(u[k] + 0.5*K1, t[k] + dt2)
        K3 = dt*f(u[k] + 0.5*K2, t[k] + dt2)
        K4 = dt*f(u[k] + K3, t[k] + dt)
        unew = u[k] + (1/6.0)*(K1 + 2*K2 + 2*K3 + K4)
        return unew

import sys, os
# BackwardEuler needs to import function Newton from Newton.py:
try:
    from Newton import Newton
except ImportError:
    pass

class BackwardEuler(ODESolver):
    """Backward Euler solver for scalar ODEs."""
    def __init__(self, f):
        ODESolver.__init__(self, f)
        # Make a sample call to check that f is a scalar function:
        try:
            u = np.array([1]); t = 1
            value = f(u, t)
        except IndexError:  # index out of bounds for u
            raise ValueError('f(u,t) must return float/int')

    # Alternative implementation of F:
    #def F(self, w):
    #    return w - self.dt*self.f(w, self.t[-1]) - self.u[self.k]
    
    def advance(self):
        u, f, k, t = self.u, self.f, self.k, self.t
        dt = t[k+1] - t[k]
        
        def F(w):
            return w - dt*f(w, t[k+1]) - u[k]

        dFdw = Derivative(F)
        w_start = u[k] + dt*f(u[k], t[k])  # Forward Euler step
        unew, n, F_value = Newton(F, w_start, dFdw, N=30)
        if k == 0:
            self.Newton_iter = []
        self.Newton_iter.append(n)
        if n >= 30:
            print "Newton's failed to converge at t=%g "\
                  "(%d iterations)" % (t[k+1], n)
        return unew


class Derivative:
    def __init__(self, f, h=1E-9):
        self.f = f
        self.h = float(h)

    def __call__(self, x):
        f, h = self.f, self.h      # make short forms
        return (f(x+h) - f(x-h))/(2*h)


# Tests and verifications

def _f1(u, t):
    return 0.2 + (u - _u_solution_f1(t))**5

def _u_solution_f1(t):
    """Exact u(t) corresponding to _f1 above."""
    return 0.2*t + 3

def _verify(f, exact, U0, T, n):
    t_points = np.linspace(0, T, n)
    for method_class in ForwardEuler, RungeKutta4, BackwardEuler:
        try:
            method = method_class(f)
        except:
            continue
        method.set_initial_condition(U0)
        u, t = method.solve(t_points)
        print method_class.__name__,
        u_exact = np.asarray(exact(t)).transpose()
        print 'max error:', (u_exact - u).max()
        if method_class is BackwardEuler:
            print 'Backward Euler iterations:', method.Newton_iter

if __name__ == '__main__':
    print 'Exact numerical solution:'
    _verify(_f1, _u_solution_f1, U0=3, T=8, n=4)
    print '\nOscillating system:'
    _verify(f=lambda u, t: [u[1], -u[0]],
            exact=lambda t: [np.sin(t), np.cos(t)],
            U0=[0,1], T=4*np.pi, n=20*4)
