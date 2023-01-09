from sympy import *
from mpmath import *
import sympy as sp
import scipy as sc
import matplotlib.pyplot as plt
from scipy.integrate import dblquad
xi, eta, rb1, rb2 = sp.symbols("xi eta rb_1 rb_2")
coordinates = Matrix([[0,0],[1,0.1],[1.2,1.2],[0.2,1]])
t = 1
print("Plane Stress")
E = 1
nu = 0
Cc = E/(1+nu)/(1-nu)*Matrix([[1,nu,0],[nu,1,0],[0,0,(1-nu)/2]])
a = coordinates.row_insert(4, Matrix([[0,0]]))
fig = plt.figure()
ax = fig.add_subplot(111)
cp = ax.plot(a[:,0], a[:,1])
ax.grid(True, which='both')
plt.xlabel("x")
plt.ylabel("y")
plt.title("Shape")
ax.axhline(y = 0, color = 'k')
ax.axvline(x = 0, color = 'k')
Shapefun = Matrix([[(1-xi)*(1-eta)],[(1+xi)*(1-eta)],
                   [(1+xi)*(1+eta)],[(1-xi)*(1+eta)]])/4
Nn = Matrix([[0 for x in range(8)] for y in range(2)])
for i in range(4):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]
x = sum([Shapefun[i]*coordinates[i,0] for i in range(4)])
y = sum([Shapefun[i]*coordinates[i,1] for i in range(4)])
J = Matrix([[x.diff(xi), x.diff(eta)],[y.diff(xi),y.diff(eta)]])
JinvT = J.inv().transpose()
mat1 = Matrix([[1,0,0,0],[0,0,0,1],[0,1,1,0]])
mat2 = Matrix([[JinvT[0,0],JinvT[0,1],0,0], 
               [JinvT[1,0],JinvT[1,1],0,0], 
               [0,0,JinvT[0,0],JinvT[0,1]], 
               [0,0,JinvT[1,0],JinvT[1,1]]])
mat3 = Matrix([[0 for x in range(8)] for y in range(4)])
for i in range(4):
    mat3[0,2*i] = mat3[2,2*i+1] = sp.diff(Shapefun[i],xi)
    mat3[1,2*i] = mat3[3,2*i+1] = sp.diff(Shapefun[i],eta)
B = mat1*mat2*mat3
k1 = B.transpose()*Cc*B
K = zeros(8)
for i in range(8):
    for j in range(8):
        integrand=k1[i,j]*J.det()
        intlam=lambdify((eta,xi),integrand)
        #mp.dps is for integral accuracy (# of decimal points)
        mp.dps = 3
        K[i,j] = quad(intlam,[-1,1],[-1,1])
# Scipy method, results in ZeroDivsionError
# K = sc.zeros(k1.shape, dtype = float)
# for (i,j), expr in sc.ndenumerate(k1):
#     tmp = sp.lambdify((xi, eta), expr*J.det(), 'math')
#     K[i,j] = dblquad(tmp, -1, 1, lambda xi: -1, lambda xi:1)[0]
print(K)
rb = Matrix([rb1,rb2])
fbeforeintegration = Nn.transpose()*rb*J.det()
f3 = Matrix([integrate(fbeforeintegration[i],(xi,-1,1),(eta,-1,1))for i in range(8)])
print("Nodal Forces: ", f3)
