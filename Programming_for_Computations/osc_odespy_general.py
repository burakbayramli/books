"""Use odespy to solve general oscillation ODEs."""

import odespy
from matplotlib.pyplot import \
  plot, savefig, legend, xlabel, figure, title, hold, axis, show

def compare(odespy_methods, f, s, F, m, U_0, V_0, T, dt,
            start_of_plot=0, umin=None, umax=None,
            exact_solution=None):
    from numpy import linspace, zeros

    def rhs(sol, t, m, f, s, F):
        # This function will remember the variables in the compare
        # function, such as m, F, s, and f, even when called from
        # odespy
        v, u = sol
        return [(1./m)*(F(t) - s(u) - f(v)),
                v]

    # If odespy_methods is not a list, but just the name of
    # a single Odespy solver, we wrap that name in a list
    # so we always have odespy_methods as a list
    if type(odespy_methods) != type([]):
        odespy_methods = [odespy_methods]

    # Make a list of solver objects
    solvers = [method(rhs, f_args=[m, f, s, F]) for method in
               odespy_methods]
    for solver in solvers:
        solver.set_initial_condition([V_0, U_0])

    # Compute the time points where we want the solution
    dt = float(dt)  # avoid integer division
    N_t = int(round(T/dt))
    time_points = linspace(0, N_t*dt, N_t+1)

    # Convert start_of_plot to index m: m*dt = start_of_plot
    m = int(round(start_of_plot/dt))

    legends = []
    for solver in solvers:
        sol, t = solver.solve(time_points)
        v = sol[:,0]
        u = sol[:,1]

        if len(solvers) == 1:
            plot(t[m:], u[m:], 'b-')  # blue line without markers
        else:
            plot(t[m:], u[m:]) # automatic line marker/color
        hold('on')
        legends.append(solver.name())
        xlabel('t')
    # Plot exact solution if available
    if exact_solution:
        plot(t[m:], exact_solution(t[m:]), 'k--')
        legends.append('exact')
    legend(legends, loc='lower left')
    if umin is not None and umax is not None:
        axis([t[m:], t[-1], umin, umax])
    savefig('tmp.pdf'); savefig('tmp.png')
    show()

def undamped_linear():
    omega = 2
    number_of_periods = 60
    time_intervals_per_period = 20

    from numpy import pi, linspace, cos
    P = 2*pi/omega                      # length of one period
    dt = P/time_intervals_per_period
    T = number_of_periods*P

    methods = [odespy.EulerCromer]
    X_0 = 2
    compare(methods,
            f=lambda v: 0,
            s=lambda u: omega**2*u,
            F=lambda t: 0,
            m=1, U_0=X_0, V_0=0, T=T, dt=dt,
            start_of_plot=T-6*P,
            umin=-2*X_0, umax=2*X_0,
            exact_solution=lambda t: X_0*cos(omega*t))

def sliding_friction():
    from numpy import tanh, sign

    f = lambda v: mu*m*g*sign(v)
    alpha = 60.0
    s = lambda u: k/alpha*tanh(alpha*u)
    #s = lambda u: k*u
    F = lambda t: 0

    g = 9.81
    mu = 0.4
    m = 1
    k = 1000

    U_0 = 0.1
    V_0 = 0

    T = 1
    dt = T/5000.

    methods = [odespy.EulerCromer, odespy.ForwardEuler]
    compare(methods, f=f, s=s, F=F, m=1, U_0=U_0, V_0=V_0,
            T=T, dt=dt, start_of_plot=0)

#undamped_linear()
sliding_friction()
