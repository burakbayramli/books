from scitools.std import *
import ODESolver

v0 = 0.05
dtau = 0.05
T = 10
n = int(round(T/dtau))
t_points = linspace(0, T, n+1)
method = ODESolver.RungeKutta4(lambda v, tau: v*(1-v))
method.set_initial_condition(v0)
v, tau = method.solve(t_points)
plot(tau, v, title='Scaled logistic equation',
     hardcopy='tmp_logistic.eps')

def u_and_t(v, tau, alpha, R):
    return alpha*tau, R*v

figure()
for alpha in linspace(0.2, 1, 5):
    t, u = u_and_t(v, tau, alpha, R=1)
    plot(t, u, legend='alpha=%g' % alpha)
    hold('on')
axis([0, T, 0, 1.1])
hardcopy('tmp_logistic_u.eps')
