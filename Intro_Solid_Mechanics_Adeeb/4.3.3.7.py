from sympy import *
import sympy as sp
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
sp.init_printing(use_latex="mathjax")
# calculation
X1, X2 = sp.symbols("X_1 X_2")
u = Matrix([0.2*X1, 0.09*X2*X1])
display("u =",u)
X = Matrix([X1, X2])
x = X + u
display("x =", x)
F = Matrix([[diff(xi,Xj) for Xj in X] for xi in x])
display("F =",F)
grad_u = F - eye(2)
display("\u2207u =",grad_u)
small_strain = (grad_u+grad_u.T)/2
display("small strain =",small_strain)
green_strain = (grad_u+grad_u.T+grad_u.T*grad_u)/2
display("green strain =",green_strain)
# plots

# vector plots
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
# contour plots
def plotContour(f, limits, title):
    x1, xn, y1, yn = limits
    dx, dy = 10/100*(xn-x1),10/100*(yn - y1)
    xrange = np.arange(x1,x1+xn,dx)
    yrange = np.arange(y1,y1+yn,dy)
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
# vector plot of u
plotVector(u,[0,2,0,2],'u' )
# small strain plots
plotContour(small_strain[0,0],[0,2,0,2],'\u03B5_small (1,1)')
plotContour(small_strain[0,1],[0,2,0,2],'\u03B5_small (1,2)')
plotContour(small_strain[1,1],[0,2,0,2],'\u03B5_small (2,2)')
# green strain plots
plotContour(green_strain[0,0],[0,2,0,2],'\u03B5_Green (1,1)')
plotContour(green_strain[0,1],[0,2,0,2],'\u03B5_Green (1,2)')
plotContour(green_strain[1,1],[0,2,0,2],'\u03B5_Green (2,2)')
