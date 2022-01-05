"""
2017NumericalMethodsOfPDE, lecture7-5
finite volume scheme

date : 2018-05-23
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

def compute_dudt(u, t, dx):

    N = u.shape[0]
    #-- if you solve different equation you only need to change f
    f = u*u*0.5  # brugers equation

    f_int = np.zeros(N+1, np.double)
    #-- we need to modify this
    # central flux reconstruction
    f_int[1:-1] = 0.5 * (f[:-1]+f[1:])

    dudt = (f_int[:-1] - f_int[1:]) / dx

    return dudt

if __name__=="__main__":

    N = 1000
    x = np.linspace(0,1,N+1)
    dx = x[1:] - x[:-1]
    u_int = -1/(2*np.pi) * np.cos(2*np.pi*x)
    u = (u_int[1:] - u_int[:-1]) / dx
    #plt.plot([x[1:],x[:-1]], [u,u], "k")

    t = np.linspace(0, 2, 201)
    sol = odeint(compute_dudt, u, t, args=(dx,))

    fig, ax = plt.subplots(1,1, figsize=(6,3), dpi=150)

    ax.plot([x[1:],x[:-1]], [sol[0,:],sol[0,:]], "k", linewidth=0.5)
    plt.pause(0.1)
    for i, _ in enumerate(t[1:]):
        ax.clear()
        ax.plot([x[1:],x[:-1]], [sol[i,:],sol[i,:]], "k", linewidth=0.5)
        plt.pause(0.1)
    #plt.plot([x[1:],x[:-1]], [u,u], "k")
