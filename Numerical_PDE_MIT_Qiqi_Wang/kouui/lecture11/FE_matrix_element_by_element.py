"""
2017NumericalMethodsOfPDE, lecture11-5
finite element solution of the matrix form of the weak form,
zero Neumann boundary condition

date : 2018-06-16
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt

if __name__ == "__main__":

    N = 20
    x = np.random.uniform(low=0.0, high=1.0, size=N+1)
    x.sort()
    x[0] = 0.0; x[-1] = 1.0
    dx = x[1:] - x[:-1]

    A = np.zeros((N+1,N+1))
    for k in range(N+1-1):
        val = 1/dx[k]
        A_k = np.array([[-val,val],[val,-val]])
        A[k:k+2,k:k+2] += A_k[:,:]

    #print(A_all)

    b = np.zeros(N+1)
    for k in range(N+1-1):
        val = 0.5 * dx[k]
        b_k = np.array([val,val])
        b[k:k+2] += b_k[:]

    print(b)

    u = np.zeros(N+1)

    #key = "zero Dirichlet"
    #key = "nonzero Dirichlet"
    #key = "zero Neumann"
    key = "nonzero Neumann"

    if key == "zero Dirichlet":
        u[1:-1] = np.linalg.solve(A[1:-1,1:-1], -b[1:-1])
    elif key == "nonzero Dirichlet":
        # left boundary equals to 1
        u[0] = 1; b[1:-1] += u[0] * A[1:-1,0]
        u[1:-1] = np.linalg.solve(A[1:-1,1:-1], -b[1:-1])
    elif key == "zero Neumann":
        # right boundary equals to -1
        u[-1] = -1; b[:-1] += u[-1] * A[:-1,-1]
        # left zero Neumann
        u[:-1] = np.linalg.solve(A[:-1,:-1], -b[:-1])
    elif key == "nonzero Neumann":
        # right boundary equals to -1
        u[-1] = -1; b[:-1] += u[-1] * A[:-1,-1]
        # left non zero Neumann
        b[0] += -1
        u[:-1] = np.linalg.solve(A[:-1,:-1], -b[:-1])
    else :
        assert False, "unknown key!"



    fig, ax = plt.subplots(1,1, figsize=(7,5), dpi=100)
    ax.plot(x, u, "o", markersize=8,
    markerfacecolor="None",markeredgecolor='black',markeredgewidth=1)
    ax.set_xlabel("x")
    ax.set_ylabel("u")
    ax.set_title("1D Poisson's Equation solve by Finite Element")
    plt.show()
