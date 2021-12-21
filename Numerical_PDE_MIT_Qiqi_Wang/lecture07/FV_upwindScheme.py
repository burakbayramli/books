"""
2017NumericalMethodsOfPDE, lecture7-5
finite volume scheme with upwind scheme to solve the non-physical oscillation around shock wave

date : 2018-05-24
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

def compute_dudt(u, t, dx):

    N = u.shape[0]
    #-- if you solve different equation you only need to change f
    f = u*u*0.5  # brugers equation
    dfdu = u

    f_int = np.zeros(N+1, np.double)

    #-- central scheme
    #f_int[1:-1] = 0.5 * (f[:-1]+f[1:])

    #-- upwind scheme
    speed = (f[:-1]-f[1:]) / (u[:-1]-u[1:])
    special_case = abs((u[:-1]-u[1:])) < 1E-8
    speed[special_case] = dfdu[:-1][special_case]
    fL = f[:-1]
    fR = f[1:]
    f_int[1:-1] = fL * (speed>0) + fR * (speed<0)

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
    ax.set_ylim(-1,1)
    plt.pause(0.1)
    for i, _ in enumerate(t[1:]):
        ax.clear()
        ax.plot([x[1:],x[:-1]], [sol[i,:],sol[i,:]], "k", linewidth=0.5)
        ax.set_ylim(-1,1)
        plt.pause(0.05)
    #plt.plot([x[1:],x[:-1]], [u,u], "k")
