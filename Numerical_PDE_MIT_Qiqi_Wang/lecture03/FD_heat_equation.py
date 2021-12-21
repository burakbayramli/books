"""
2017NumericalMethodsOfPDE, lecture3-3
finite difference for heat equation

date : 2018-05-19
author: kouui
"""

import matplotlib.pyplot as plt
import numpy as np



if __name__ == '__main__':
    N = 30
    x = np.linspace(0, 1, N+1)
    dx = x[1] - x[0]
    temp_ = np.ones(N-2)
    A = np.eye(N-1) * (-2/dx**2) + np.diag(temp_, +1) * 1/dx**2 + np.diag(temp_, -1) * 1/dx**2
    B = np.zeros(N-1).reshape(-1,1)
    Kappa = 0.01
    u = np.exp(-(x-0.5)**2 * 1E2).reshape(-1,1)

    dt = dx*dx/Kappa/2  # criterion of explicit Euler method
    for it, tt in enumerate(np.arange(dt,10+dt,dt)):
        dudt = Kappa * (A @ u[1:-1]) + Kappa * B
        u[1:-1] += dudt * dt    # integrate using Euler's scheme
        plt.plot(x,u, "k")
        plt.pause(dt)
