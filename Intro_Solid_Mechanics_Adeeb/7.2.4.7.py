from sympy import *
import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
E, e33, v, s11, s22, X1, X2, X3= symbols("E \u03b5_{33} \u03bd \u03c3_{11} \u03c3_{22} X_1 X_2 X_3")

def plotVector(f, limits, title):
    fx, fy = [lambdify((X1,X2),fi) for fi in f]
    x1, xn, y1, yn = limits
    dx, dy = 10/100*(xn-x1),10/100*(yn-y1)
    xrange = np.arange(x1,xn,dx)
    yrange = np.arange(y1,yn,dy)
    X, Y = np.meshgrid(xrange, yrange)
    dxplot, dyplot = fx(X,Y), fy(X,Y)
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.quiver(X, Y, dxplot, dyplot)
    plt.title(title)

def plotContour(f, limits, title):
    x1, xn, y1, yn = limits
    dx, dy = 10/100*(xn-x1),10/100*(yn - y1)
    xrange = np.arange(x1,xn,dx)
    yrange = np.arange(y1,yn,dy)
    X, Y = np.meshgrid(xrange, yrange)
    lx, ly = len(xrange), len(yrange)
    F = lambdify((X1,X2),f)
    # if F is constant, then generates array
    # if F generates array, doesn't change anything
    Z = F(X,Y)*np.ones(lx*ly).reshape(lx, ly)
    fig = plt.figure()
    ax = fig.add_subplot(111)
    cp = ax.contourf(X,Y,Z)
    fig.colorbar(cp)
    plt.title(title)

X = Matrix([[X1], [X2], [X3]])
u = Matrix([[0.02*X1 + 0.02*X1*X2], [0.0015*X2*X1**2 + 0.02*X2]])
limits = [0,5,0,1]
plotVector(u,limits, "u")
gradu = Matrix([[diff(ui,Xj) for Xj in X[:2]] for ui in u])
display("plane gradient for displacement function: ", gradu)
esmall = 1/2 * (gradu + Transpose(gradu))
strvector = Matrix([esmall[0,0], esmall[1,1], 2*esmall[0,1]])
Ee = 1000
Nu = 0.4
Cc = Matrix([[1/(1-Nu), Nu/(1-Nu), 0],
             [Nu/(1-Nu), 1/(1-Nu), 0],
             [0,0,1/2]])
Cc = Ee/(1+Nu) * Cc
stressvector = Cc * strvector
display("stressvector: ", stressvector)
strain = Matrix([[esmall[0,0], esmall[0,1], 0],
                 [esmall[1,0], esmall[1,1], 0],
                 [0, 0, -Nu/Ee * (stressvector[0] + stressvector[1])]])
thickness = 1
delta = strain[2,2] * thickness
display("strain component: ")
display(Eq(e33, v/E*(s11+s22)))
display(Eq(e33, delta))
plotContour(delta, limits, "delta thickness mm")
