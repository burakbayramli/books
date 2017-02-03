"""Class(es) implementing the Forward Euler method for scalar ODEs."""

import numpy as np

class ForwardEuler_v1:
    """
    Class for solving an ODE,

      du/dt = f(u, t)

    by the ForwardEuler method.
    
    Class attributes:
    t: array of time values
    u: array of solution values (at time points t)
    n: number of time steps in the simulation
    k: step number of the most recently computed solution
    f: callable object implementing f(u, t)
    dt: time step (assumed constant)
    """
    def __init__(self, f, U0, T, n):
        if not callable(f):
            raise TypeError('f is %s, not a function' % type(f))
        
        self.f, self.U0, self.T, self.n = f, dt, U0, T, n
        self.dt = T/float(n)
        self.u = np.zeros(self.n+1)  
        self.t = np.zeros(self.n+1)
        
    def solve(self):
        """Compute solution for 0 <= t <= T."""
        self.u[0] = float(self.U0)
        self.t[0] = float(0)

        for k in range(self.n):
            self.k = k
            self.t[k+1] = self.t[k] + self.dt
            self.u[k+1] = self.advance()
        return self.u, self.t

    def advance(self):
        """Advance the solution one time step."""
        # Load attributes into local variables to
        # obtain a formula that is as close as possible
        # to the mathematical notation.
        u, dt, f, k, t = \
           self.u, self.dt, self.f, self.k, self.t

        unew = u[k] + dt*f(u[k], t[k])
        return unew

class ForwardEuler:
    """
    Class for solving an ODE,

      du/dt = f(u, t)

    by the ForwardEuler method.
    
    Class attributes:
    t: array of time values
    u: array of solution values (at time points t)
    k: step number of the most recently computed solution
    f: callable object implementing f(u, t)
    dt: time step (assumed constant)
    """
    def __init__(self, f):
        if not callable(f):
            raise TypeError('f is %s, not a function' % type(f))
        self.f = f

    def set_initial_condition(self, U0):
        self.U0 = float(U0)

    def solve(self, time_points):
        """Compute u for t values in time_points list."""
        self.t = np.asarray(time_points)
        self.u = np.zeros(len(time_points))
        # Assume self.t[0] corresponds to self.U0
        self.u[0] = self.U0
        
        for k in range(len(self.t)-1):
            self.k = k
            self.u[k+1] = self.advance()
        return self.u, self.t

    def advance(self):
        """Advance the solution one time step."""
        # Load attributes into local variables to
        # obtain a formula that is as close as possible
        # to the mathematical notation.
        u, f, k, t = self.u, self.f, self.k, self.t

        dt = t[k+1] - t[k]
        unew = u[k] + dt*f(u[k], t[k])
        return unew


def _f1(u, t):
    return 0.2 + (u - _u_solution_f1(t))**4

def _u_solution_f1(t):
    return 0.2*t + 3

def _verify_f1_ForwardEuler_v1():
    U0 = 3;  dt = 0.4;  T = 3
    method = ForwardEuler_v1(_f1, dt, U0, T)
    u, t = method.solve()
    u_exact = _u_solution_f1(t)
    print 'Numerical:', u
    print 'Exact:    ', u_exact

def _verify_f1_ForwardEuler():
    U0 = 3
    method = ForwardEuler(_f1)
    method.set_initial_condition(U0)
    t = [0, 0.4, 1, 1.2]
    u1, t1 = method.solve(t)
    # Continue with a new time interval
    method.set_initial_condition(u1[-1])
    t = [1.2, 1.4, 1.5]
    u2, t2 = method.solve(t)
    u = np.concatenate((u1, u2))
    t = np.concatenate((t1, t2))
    u_exact = _u_solution_f1(t)
    print 'time values:', t
    print 'Numerical:  ', u
    print 'Exact:      ', u_exact

def u_exp():
    def f(u, t):
        return u

    method = ForwardEuler_v1(f, dt=0.1, U0=1, T=3)
    u, t = method.solve()
    
class Logistic:
    """Problem class for a logistic ODE."""
    def __init__(self, alpha, R, U0):
        self.alpha, self.R, self.U0 = alpha, float(R), U0

    def __call__(self, u, t):
        """Return f(u,t) for the logistic ODE."""
        return self.alpha*u*(1 - u/self.R)

    def __str__(self):
        """Return ODE and initial condition."""
        return "u'(t) = %g*u*(1 - u/%g)\nu(0)=%g" % \
               (self.alpha, self.R, self.U0)
    
def logistic():
    problem = Logistic(alpha=0.2, R=1, U0=0.1)
    T = 40
    method = ForwardEuler(problem)
    method.set_initial_condition(problem.U0)
    t = np.linspace(0, T, 401)  # 400 intervals in [0,T]
    u, t = method.solve(t)
    # Note that import * is not legal inside functions so we
    # have to import each specific function.
    from scitools.std import plot, hardcopy, xlabel, ylabel, title
    plot(t, u)
    xlabel('t'); ylabel('u')
    title('Logistic growth: alpha=%s, R=%g, dt=%g' \
          % (problem.alpha, problem.R, t[1]-t[0]))
    # Compare with exponential growth
    #from scitools.std import hold, linspace, exp
    #te = linspace(0, dt*N, N+1)
    #ue = 0.1*exp(0.2*te)
    #hold('on')
    #plot(te, ue)
    hardcopy('tmp.eps')
    
if __name__ == '__main__':
    import sys
    try:
        command = sys.argv[1]
    except IndexError:
        print 'provide a command (verify, logistic, ...)'
        sys.exit(1)
        
    if command == 'verify_v1':
        _verify_f1_ForwardEuler_v1()
    elif command == 'verify':
        _verify_f1_ForwardEuler()
    elif command == 'logistic':
        logistic()
