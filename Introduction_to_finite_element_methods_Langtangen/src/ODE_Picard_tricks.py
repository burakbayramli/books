"""
Solve u' = f(u, t). Test if a trick in linearization in a Picard
iteration is done like f(u_,t)*u_/u, if u_ is the most recent
approximation to u (called Picard2 in the Odespy software).
"""
from odespy import BackwardEuler

from numpy import cos, linspace, exp, sin

def f(u, t):
    return sin(2*(1+u))

def f2(u, t):
    return -u**3

eps_iter = 0.001
max_iter = 500
solver1 = BackwardEuler(f, nonlinear_solver='Picard', verbose=2,
                        eps_iter=eps_iter, max_iter=max_iter)
solver2 = BackwardEuler(f, nonlinear_solver='Picard2', verbose=2,
                        eps_iter=eps_iter, max_iter=max_iter)
solver1.set_initial_condition(1)
solver2.set_initial_condition(1)
tp = linspace(0, 4, 11)
u1, t = solver1.solve(tp)
u2, t = solver2.solve(tp)
print('Picard it:', solver1.num_iterations_total)
print('Picard2 it:', solver2.num_iterations_total)

import matplotlib.pyplot as plt
plt.plot(t, u1, t, u2, legend=('Picard', 'Picard2'))
plt.show()

"""
f(u,t) = -u**3:

BackwardEuler.advance w/Picard: t=0.4, n=1:   u=0.796867 in 22 iterations
BackwardEuler.advance w/Picard: t=0.8, n=2:   u=0.674568 in 9 iterations
BackwardEuler.advance w/Picard: t=1.2, n=3:   u=0.591494 in 6 iterations
BackwardEuler.advance w/Picard: t=1.6, n=4:   u=0.531551 in 5 iterations
BackwardEuler.advance w/Picard: t=2, n=5:   u=0.485626 in 4 iterations
BackwardEuler.advance w/Picard: t=2.4, n=6:   u=0.44947 in 3 iterations
BackwardEuler.advance w/Picard: t=2.8, n=7:   u=0.419926 in 3 iterations
BackwardEuler.advance w/Picard: t=3.2, n=8:   u=0.395263 in 3 iterations
BackwardEuler.advance w/Picard: t=3.6, n=9:   u=0.374185 in 2 iterations
BackwardEuler.advance w/Picard: t=4, n=10:   u=0.356053 in 2 iterations

BackwardEuler.advance w/Picard2: t=0.4, n=1:   u=0.797142 in 8 iterations
BackwardEuler.advance w/Picard2: t=0.8, n=2:   u=0.674649 in 5 iterations
BackwardEuler.advance w/Picard2: t=1.2, n=3:   u=0.591617 in 4 iterations
BackwardEuler.advance w/Picard2: t=1.6, n=4:   u=0.531506 in 4 iterations
BackwardEuler.advance w/Picard2: t=2, n=5:   u=0.485752 in 3 iterations
BackwardEuler.advance w/Picard2: t=2.4, n=6:   u=0.44947 in 3 iterations
BackwardEuler.advance w/Picard2: t=2.8, n=7:   u=0.419748 in 2 iterations
BackwardEuler.advance w/Picard2: t=3.2, n=8:   u=0.395013 in 2 iterations
BackwardEuler.advance w/Picard2: t=3.6, n=9:   u=0.374034 in 2 iterations
BackwardEuler.advance w/Picard2: t=4, n=10:   u=0.355961 in 2 iterations
Picard it: 59
Picard2 it: 35

f(u,t) = exp(-u): no effect.
f(u,t) = log(1+u): no effect.

f(u,t) = sin(2*(1+u))
Calling f(U0, 0) to determine data type
BackwardEuler.advance w/Picard: t=0.4, n=1:   u=0.813754 in 17 iterations
BackwardEuler.advance w/Picard: t=0.8, n=2:   u=0.706846 in 21 iterations
BackwardEuler.advance w/Picard: t=1.2, n=3:   u=0.646076 in 20 iterations
BackwardEuler.advance w/Picard: t=1.6, n=4:   u=0.612998 in 19 iterations
BackwardEuler.advance w/Picard: t=2, n=5:   u=0.593832 in 16 iterations
BackwardEuler.advance w/Picard: t=2.4, n=6:   u=0.583236 in 14 iterations
BackwardEuler.advance w/Picard: t=2.8, n=7:   u=0.578087 in 11 iterations
BackwardEuler.advance w/Picard: t=3.2, n=8:   u=0.574412 in 8 iterations
BackwardEuler.advance w/Picard: t=3.6, n=9:   u=0.573226 in 5 iterations
BackwardEuler.advance w/Picard: t=4, n=10:   u=0.572589 in 3 iterations

BackwardEuler.advance w/Picard2: t=0.4, n=1:   u=0.813614 in 7 iterations
BackwardEuler.advance w/Picard2: t=0.8, n=2:   u=0.706769 in 9 iterations
BackwardEuler.advance w/Picard2: t=1.2, n=3:   u=0.646828 in 11 iterations
BackwardEuler.advance w/Picard2: t=1.6, n=4:   u=0.612648 in 12 iterations
BackwardEuler.advance w/Picard2: t=2, n=5:   u=0.59438 in 13 iterations
BackwardEuler.advance w/Picard2: t=2.4, n=6:   u=0.583541 in 12 iterations
BackwardEuler.advance w/Picard2: t=2.8, n=7:   u=0.577485 in 10 iterations
BackwardEuler.advance w/Picard2: t=3.2, n=8:   u=0.574147 in 8 iterations
BackwardEuler.advance w/Picard2: t=3.6, n=9:   u=0.573038 in 5 iterations
BackwardEuler.advance w/Picard2: t=4, n=10:   u=0.572446 in 3 iterations
Picard it: 134
Picard2 it: 90

"""
