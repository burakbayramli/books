"""
2017NumericalMethodsOfPDE, lecture10-5
finite element solution of the matrix form of the weak form

date : 2018-06-14
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt


if __name__ == "__main__":

    N = 40
    x = np.random.uniform(low=0.0, high=1.0, size=N+1)
    x.sort()
    x[0] = 0.0; x[-1] = 1.0
    Adiag = - (1/(x[1:-1]-x[:-2]) + 1/(x[2:]-x[1:-1]))
    Aoffd = 1/(x[2:-1]-x[1:-2])
    A = np.diag(Adiag) + np.diag(Aoffd,+1) + np.diag(Aoffd,-1)
    b = 0.5 * (x[2:]-x[:-2])

    u = np.zeros(N+1)
    u[1:-1] = np.linalg.solve(A, -b)

    fig, ax = plt.subplots(1,1, figsize=(7,4), dpi=100)
    ax.plot(x, u, "*", markersize=5)
    ax.set_xlabel("x")
    ax.set_ylabel("u")
    ax.set_title("1D Poisson's Equation solve by Finite Element")
    plt.show()
