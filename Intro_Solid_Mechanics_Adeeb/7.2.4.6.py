from sympy import *
import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
sp.init_printing(use_latex = "mathjax")
def vonMises(M):
    return sp.sqrt(1/2*((M[0,0] - M[1,1])**2+(M[1,1] - M[2,2])**2+
                       (M[2,2] - M[0,0])**2+6*(M[0,1]**2+M[0,2]**2+M[1,2]**2)))

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

x, x1, x2, x3, X1, X2, X3, u, pb1, pb2, pb3= symbols("x x_1 x_2 x_3 X_1 X_2 X_3 u pb_1 pb_2 pb_3")
x1 = X1 + 0.001*X2 + 0.0002*X1**2
x2 = 1.001*X2 - 0.0002*X2**2
x3 = X3
print("displacement function: ")
X = Matrix([[X1], [X2], [X3]])
x = Matrix([[x1], [x2], [x3]])
u = x - X
print(u)
limits = [0,2,0,1]
uplane = u[:2]
print(uplane)
plotVector(uplane, limits, "u")
gradu = Matrix([[diff(ui,Xj) for Xj in X] for ui in u])
print("displacement tensor gradient", gradu)
strGreen = 1/2 * (gradu + Transpose(gradu) + Transpose(gradu) * gradu)
strain = 1/2 * (gradu + Transpose(gradu))
print("engineering strain matrix: ", strain)
print("Green strain matrix: ", strGreen)
strainvector = Matrix([[strain[0,0]], [strain[1,1]], [strain[2,2]], [2 * strain[0,1]], [2 * strain[0,2]                       ], [2 * strain[1, 2]]])
print("small strain vector: ", strainvector)
Ee = 210000
Nu = 0.3
G = Ee/2/(1+Nu)
Cc = Matrix([[1/Ee, -Nu/Ee, -Nu/Ee, 0, 0, 0],
             [-Nu/Ee, 1/Ee, -Nu/Ee, 0, 0, 0], 
             [-Nu/Ee, -Nu/Ee, 1/Ee, 0, 0, 0],
             [0, 0, 0, 1/G, 0, 0], 
             [0, 0, 0, 0, 1/G, 0],
             [0, 0, 0, 0, 0, 1/G]])
Dd = Inverse(Cc)
stressvector = Dd * strainvector
print("stressvector: ", stressvector)     
stressmatrix = Matrix([[stressvector[0], stressvector[3], stressvector[4]],
                       [stressvector[3], stressvector[1], stressvector[5]],
                       [stressvector[4], stressvector[5], stressvector[2]]])
print("stressmatrix: ", stressmatrix)
s = stressmatrix
sum1 = -sum(diff(s[i,0], X[i]) for i in range(3)) 
print(Eq(pb1, sum1))
sum2 = -sum(diff(s[i,1], X[i]) for i in range(3)) 
print(Eq(pb2, sum2))
sum3 = sum(diff(s[i,2], X[i]) for i in range(3)) 
print(Eq(pb3, sum3))
a = vonMises(stressmatrix)
print("von Mises stress: ", a)
plotContour(a,limits,"vonMises")
plotContour(s[2,2],limits, "s33")
