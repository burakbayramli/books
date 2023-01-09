from sympy import *
from numpy import *
import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
A, L, x , EA, E = symbols("A L x EA E")

def plotContour(f, limits, title):
    x1, xn, y1, yn = limits
    dx, dy = 10/100*(xn-x1),10/100*(yn - y1)
    xrange = np.arange(x1,xn+dx,dx)
    yrange = np.arange(y1,yn+dy,dy)
    X, Y = np.meshgrid(xrange, yrange)
    lx, ly = len(xrange), len(yrange)
    F = lambdify((x,X2),f)
    # if F is constant, then generates array
    # if F generates array, doesn't change anything
    Z = F(X,Y)*np.ones(lx*ly).reshape(lx, ly)
    fig = plt.figure(figsize = (8,2))
    ax = fig.add_subplot(111)
    cp = ax.contourf(X,Y,Z)
    fig.colorbar(cp)
    plt.title(title)

def plotVector(f, limits, title):
    fx, fy = [lambdify((x,X2),fi) for fi in f]
    x1, xn, y1, yn = limits
    dx, dy = 10/100*(xn-x1),10/100*(yn-y1)
    xrange = np.arange(x1,xn,dx)
    yrange = np.arange(y1,yn,dy)
    X, Y = np.meshgrid(xrange, yrange)
    dxplot, dyplot = fx(X,Y), fy(X,Y)
    fig = plt.figure(figsize = (7.5,2))
    ax = fig.add_subplot(111)
    ax.quiver(X, Y, dxplot, dyplot)
    plt.title(title)

u = Function("u")
EA = E * A
u1 = u(x).subs(x,0)
BC2 = u(x).diff(x).subs(x,L)
Normalf = EA * u(x).diff(x)
qnormal = 125 * x/L
print("Only non-zero stress component: ", Normalf)
print("Distributed axial Load: ", qnormal)
s = dsolve(EA * u(x).diff(x,2) + qnormal, u(x), ics = {u1:0, BC2:0})
u = s.rhs
s11 = E * u.diff(x)
Normalf = EA * u.diff(x)
print("u_1, sigma_11 and Normal force: ", u, s11, Normalf)
s11 = s11.subs({E:20000, A:0.125, L:5})
u = u.subs({E:20000, A:0.125, L:5})
disp = Matrix([[u], [0]])
plotContour(s11, [0,5,-0.25,0.25], "sigma_11")
plotVector(disp, [0,5,-0.25,0.25], "Displacement Vector")
