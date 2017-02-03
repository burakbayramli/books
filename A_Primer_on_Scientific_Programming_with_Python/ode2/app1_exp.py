from scitools.std import *
import ODESolver

def f(u, t):
    return u

method = ODESolver.ForwardEuler(f)
method.set_initial_condition(1.0)
t_points = linspace(0, 3, 31)
u, t = method.solve(t_points)
plot(t, u)

# Test more n values and plot
figure()
T = 3
for dt in 0.1, 0.5, 1.0:
    n = int(round(T/dt))
    m = ODESolver.ForwardEuler(f)
    m.set_initial_condition(1)
    u, t = m.solve(linspace(0, T, n+1))
    plot(t, u)
    legend('dt=%g' % dt)
    hold('on')


t = linspace(0, T, 41)   # finer resolution for exact solution
u_exact = exp(t)
plot(t, u_exact)
legend('exact')
title("u'=u solved by the Forward Euler method")
savefig('tmp_uexp1.eps')

# Test ForwardEuler vs RungeKutta4
T = 3
dt = 1
n = int(round(T/dt))
t_points = linspace(0, T, n+1)
figure()
for method_class in ODESolver.ForwardEuler, ODESolver.RungeKutta4:
    method = method_class(f)
    method.set_initial_condition(1)
    u, t = method.solve(t_points)
    plot(t, u)
    legend('%s' % method_class.__name__)
    hold('on')

t = linspace(0, T, 41)  # finer resolution for exact solution
plot(t, u_exact)
legend('exact')
title("u'=u solved numerically")
savefig('tmp_uexp2.eps')
