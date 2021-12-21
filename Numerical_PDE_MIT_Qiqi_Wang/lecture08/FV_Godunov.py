"""
2017NumericalMethodsOfPDE, lecture8-5
finite volume with Godunov scheme with burger's equation

date : 2018-06-13
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

def compute_dudt(u, t):

    N = u.shape[0]

    #--
    # if you solve different equation you only need to change f
    f = u*u*0.5  # brugers equation
    #--

    f_int = np.zeros(N+1, np.double)

    #--
    # you only need to modify this to have different flux reconstruction scheme
    uL = u[:-1]; uR = u[1:]
    maxF = np.maximum(f[:-1],f[1:])
    minF = np.minimum(f[:-1],f[1:])
    minF[np.logical_and(uR>0,uL<0)] = 0
    f_int[1:-1] = maxF * (uL>uR) + minF * (uL<=uR)
    f_int[0] = f[0]; f_int[-1] = f[-1]
    #--

    dudt = (f_int[:-1] - f_int[1:]) / dx

    return dudt

if __name__ == "__main__":

    N = 100
    x = np.linspace(0,1,N+1)
    dx = x[1:] - x[:-1]

    u = np.ones(N)
    u[:int(N/2)] = -1
    u0 = u

    t = np.linspace(0, 2, 201)
    #sol = odeint(compute_dudt, u, t, args=(dx,))
    sol = odeint(compute_dudt, u, t)

    fig, ax = plt.subplots(1,1, figsize=(6,3), dpi=150)
    ax.plot([x[1:],x[:-1]], [sol[0,:],sol[0,:]], "k", linewidth=0.5)
    #ax.set_ylim(-1.1,1.1)
    plt.pause(0.1)
    for i, _ in enumerate(t[1:]):
        ax.clear()
        ax.plot([x[1:],x[:-1]], [sol[i,:],sol[i,:]], "k", linewidth=0.5)
        #ax.set_ylim(-1.1,1.1)
        plt.pause(0.1)
