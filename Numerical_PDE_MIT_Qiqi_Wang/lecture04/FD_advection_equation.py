"""
2017NumericalMethodsOfPDE, lecture4-7
finite difference for advection equation

!! the peak is increasing --> unstable with forward Euler ODE solver
!! shape is changing --> error accumulate

date : 2018-05-19
author: kouui
"""

import matplotlib.pyplot as plt
import numpy as np



if __name__ == '__main__':
    N = 100
    x = np.linspace(0, 1, N+1)
    dx = x[1] - x[0]
    temp_ = np.ones(N-1)
    A = np.diag(temp_, +1) * (-1)/(2*dx) + np.diag(temp_, -1) * (+1)/(2*dx)
    A[0,-1] = (+1)/(2*dx)
    A[-1,0] = (-1)/(2*dx)
    U = 0.5
    u = np.exp(-(x-0.5)**2 * 1E2).reshape(-1,1)

    dt = 0.01
    for it, tt in enumerate(np.arange(dt,10+dt,dt)):
        dudt = U * (A @ u[:-1,0])
        u[:-1,0] += dudt * dt    # integrate using Euler's scheme
        u[-1,0] = u[0,0]
        plt.plot(x,u, "k")
        plt.pause(dt)
