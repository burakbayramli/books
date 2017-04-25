from matplotlib.pyplot import plot, hold, legend, \
     xlabel, ylabel, savefig, title, figure, show

def EulerCromer(f, s, F, m, T, U_0, V_0, dt):
    from numpy import zeros, linspace
    N_t = int(round(T/dt))
    print 'N_t:', N_t
    t = linspace(0, N_t*dt, N_t+1)

    u = zeros(N_t+1)
    v = zeros(N_t+1)

    # Initial condition
    u[0] = U_0
    v[0] = V_0

    # Step equations forward in time
    for n in range(N_t):
        v[n+1] = v[n] + dt*(1./m)*(F(t[n]) - f(v[n]) - s(u[n]))
        u[n+1] = u[n] + dt*v[n+1]
    return u, v, t

def test_undamped_linear():
    """Compare with data from osc_EC.py in a linear problem."""
    from numpy import pi
    omega = 2
    P = 2*pi/omega
    dt = P/20
    T = 40*P
    exact_v = -3.5035725322034139
    exact_u = 0.7283057044967003
    computed_u, computed_v, t = EulerCromer(
        f=lambda v: 0, s=lambda u: omega**2*u,
        F=lambda t: 0, m=1, T=T, U_0=2, V_0=0, dt=dt)
    diff_u = abs(exact_u - computed_u[-1])
    diff_v = abs(exact_v - computed_v[-1])
    tol = 1E-14
    assert diff_u < tol and diff_v < tol

def _test_manufactured_solution(damping=True):
    import sympy as sp
    t, m, k, b = sp.symbols('t m k b')
    # Choose solution
    u = sp.sin(t)
    v = sp.diff(u, t)
    # Choose f, s, F
    f = b*v
    s = k*sp.tanh(u)
    F = sp.cos(2*t)

    equation = m*sp.diff(v, t) + f + s - F

    # Adjust F (source term because of manufactured solution)
    F += equation
    print 'F:', F

    # Set values for the symbols m, b, k
    m = 0.5
    k = 1.5
    b = 0.5 if damping else 0
    F = F.subs('m', m).subs('b', b).subs('k', k)

    print f, s, F
    # Turn sympy expression into Python function
    F = sp.lambdify([t], F)
    # Define Python functions for f and s
    # (the expressions above are functions of t, we need
    # s(u) and f(v)
    from numpy import tanh
    s = lambda u: k*tanh(u)
    f = lambda v: b*v

    # Add modules='numpy' such that exact u and v work
    # with t as array argument
    exact_u = sp.lambdify([t], u, modules='numpy')
    exact_v = sp.lambdify([t], v, modules='numpy')


    # Solve problem for different dt
    from numpy import pi, sqrt, sum, log
    P = 2*pi
    time_intervals_per_period = [20, 40, 80, 160, 240]
    h   = []  # store discretization parameters
    E_u = []  # store errors in u
    E_v = []  # store errors in v

    for n in time_intervals_per_period:
        dt = P/n
        T = 8*P
        computed_u, computed_v, t = EulerCromer(
            f=f, s=s, F=F, m=m, T=T,
            U_0=exact_u(0), V_0=exact_v(0), dt=dt)

        error_u = sqrt(dt*sum((exact_u(t) - computed_u)**2))
        error_v = sqrt(dt*sum((exact_v(t) - computed_v)**2))
        h.append(dt)
        E_u.append(error_u)
        E_v.append(error_v)

        """
        # Compare exact and computed curves for this resolution
        figure()
        plot_u(computed_u, t, show=False)
        hold('on')
        plot(t, exact_u(t), show=True)
        legend(['numerical', 'exact'])
        savefig('tmp_%d.pdf' % n); savefig('tmp_%d.png' % n)
        """
    # Compute convergence rates
    r_u = [log(E_u[i]/E_u[i-1])/log(h[i]/h[i-1])
           for i in range(1, len(h))]
    r_v = [log(E_u[i]/E_u[i-1])/log(h[i]/h[i-1])
           for i in range(1, len(h))]
    tol = 0.02
    exact_r_u = 1.0 if damping else 2.0
    exact_r_v = 1.0 if damping else 2.0
    success = abs(exact_r_u - r_u[-1]) < tol and \
              abs(exact_r_v - r_v[-1]) < tol
    msg = ' u rate: %.2f, v rate: %.2f' % (r_u[-1], r_v[-1])
    assert success, msg

def test_manufactured_solution():
    _test_manufactured_solution(damping=True)
    _test_manufactured_solution(damping=False)

# Plot the a percentage of the time series, up to the end, to
# illustrate the accuracy in long time simulations
def plot_u(u, t, percentage=100, show=True, heading='', labels=('t', 'u')):
    index = len(u)*percentage/100.
    plot(t[-index:], u[-index:], 'b-', show=show)
    xlabel(labels[0]);  ylabel(labels[1])
    title(heading)
    savefig('tmp.pdf'); savefig('tmp.png')
    if show:
        show()

def linear_damping():
    b = 0.3
    f = lambda v: b*v
    s = lambda u: k*u
    F = lambda t: 0

    m = 1
    k = 1
    U_0 = 1
    V_0 = 0

    T = 12*pi
    dt = T/5000.

    u, v, t = EulerCromer(f=f, s=s, F=F, m=m, T=T,
                          U_0=U_0, V_0=V_0, dt=dt)
    plot_u(u, t)

def linear_damping_sine_excitation():
    b = 0.3
    f = lambda v: b*v
    s = lambda u: k*u
    from math import pi, sin
    w = 1
    A = 0.5
    F = lambda t: A*sin(w*t)

    m = 1
    k = 1
    U_0 = 1
    V_0 = 0

    T = 12*pi
    dt = T/5000.

    u, v, t = EulerCromer(f=f, s=s, F=F, m=m, T=T,
                          U_0=U_0, V_0=V_0, dt=dt)
    plot_u(u, t)

def sliding_friction():
    from numpy import tanh, sign

    f = lambda v: mu*m*g*sign(v)
    alpha = 60.0
    s = lambda u: k/alpha*tanh(alpha*u)
    s = lambda u: k*u
    F = lambda t: 0

    g = 9.81
    mu = 0.4
    m = 1
    k = 1000

    U_0 = 0.1
    V_0 = 0

    T = 2
    dt = T/5000.

    u, v, t = EulerCromer(f=f, s=s, F=F, m=m, T=T,
                          U_0=U_0, V_0=V_0, dt=dt)
    plot_u(u, t)

if __name__ == '__main__':
    test_undamped_linear()
    test_manufactured_solution()
    #sliding_friction()
    linear_damping_sine_excitation()
