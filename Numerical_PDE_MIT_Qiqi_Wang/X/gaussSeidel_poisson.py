"""
2017NumericalMethodsOfPDE, lectureX2
solve poisson equation with Gauss-Seidel iteration method

date : 2018-06-17
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt

if __name__ == "__main__":

    n = 100
    b = np.ones(n)
    u = np.zeros(n)
    dx = 1./(n+1)

    fig, ax = plt.subplots(1,1, figsize=(7,5), dpi=100)
    ax.set_ylim(-0.2,0.001)
    for it in range(4000):
        u[0] = 0.5*u[1] - 0.5*dx*dx * b[0]
        for i in range(1,n-1):
            u[i] = 0.5*(u[i-1]+u[i+1]) - 0.5*dx*dx*b[i]
        u[n-1] = 0.5*u[n-2] - 0.5*dx*dx * b[n-1]

        if (it%100)==0:
            ax.plot(u, "k", linewidth=1)

            #plt.show(block=False) # only final result
            plt.pause(0.1)  # animation

    plt.show()
