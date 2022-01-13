# use simulated annealing to find the minimum of a function

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt
import random

def f1(x):
    return x**2 - np.cos(4.0*np.pi*x)


def f2(x):
    return np.cos(x) + np.cos(np.sqrt(2.0)*x) + np.cos(np.sqrt(3.0)*x)


fun = f2

Tmin = 1.e-3
Tmax = 1000.0
tau = 1.e4

# cooling loop
t = 0
T = Tmax

#xmin = -10.0
#xmax = 10.0

xmin = 0.0
xmax = 50.0

# initial guess
x_old = np.random.uniform(xmin, xmax)
f_old = fun(x_old)


while T > Tmin:
    T = Tmax*np.exp(-t/tau)

    # perturb x
    x_new = min(xmax, max(xmin, x_old + np.random.normal()))
    f_new = fun(x_new)

    # check whether we should keep it
    if random.random() < np.exp(-(f_new - f_old)/T):
        x_old = x_new
        f_old = f_new

    t += 1


print(x_old)


xx = np.linspace(xmin, xmax, 1000)

plt.plot(xx, fun(xx))
plt.scatter(x_old, f_old)

plt.savefig("min.png")

