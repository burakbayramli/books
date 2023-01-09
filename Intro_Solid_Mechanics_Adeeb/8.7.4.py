from sympy import *
from numpy import *
import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
a, L, x , EI, E,  I  = symbols("a L x EI Ee I")

def vonMises(M):
    return sp.sqrt(1/2*((M[0,0] - M[1,1])**2+(M[1,1] - M[2,2])**2+
                       (M[2,2] - M[0,0])**2+6*(M[0,1]**2+M[0,2]**2+M[1,2]**2)))

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

y = Function('y')
th = y(x).diff(x)
M = EI * y(x).diff(x,2)
V = EI * y(x).diff(x,3)
q = -125
M1 = M.subs(x,0)
M2 = M.subs(x,L)
V1 = V.subs(x,0)
V2 = V.subs(x,L)
y1 = y(x).subs(x,0)
y2 = y(x).subs(x,L)
th1 = th.subs(x,0)
th2 = th.subs(x,L)
s = dsolve(EI*y(x).diff(x,4) - q, y(x), ics = {M2/EI:0,y1:0,y2:0, M1/EI:0})
print("displacement function: ",s)
y = s.rhs
u = Matrix([[X2* -y.diff(x)], [y]])
M = EI * y.diff(x,2)
V = EI * y.diff(x,3) 
print("Moment: ", M.subs(L,8))
print("Shear: ", V.subs(L,8))
b = 0.25
s11 = -M*X2/I
Q = (t/2 - X2) * b * (t/4 + X2/2)
s12 = -V * Q/I/b
print("stresses: ", s11.subs(L,8), s12.subs(L,8))
s11 = s11.subs({E:20000000, t:0.5, L:8, I:b*t**3/12})
s12 = s12.subs({E:20000000, t:0.5, L:8, I:b*t**3/12})
smatrix = Matrix([[s11, s12, 0], [s12, 0, 0], [0, 0, 0]])
vonMisesstress = vonMises(smatrix)
print("von Mises stress: ", vonMisesstress)
plotContour(s11, [0,8,-0.25,0.25], "sigma_11")
plotContour(s12, [0,8,-0.25,0.25], "sigma_12")
plotContour(vonMisesstress, [0,8,-0.25,0.25], "sigma_vM")
u = u.subs({E:20000000, t:0.5, L:8, I:b*t**3/12, EI:E*I})
plotVector(u, [0,8,-0.25,0.25], "Displacement Vector")
