from sympy import *
from mpmath import *
import sympy as sp
import matplotlib.pyplot as plt
xi, eta, rb1, rb2 = sp.symbols("xi eta rb_1 rb_2")
coordinates = Matrix([[0,0],[1,0.1],[1.2,1.2],[0.2,1],[0.5,0],[1.1,0.65],[0.7,1.2],[0.1,0.5]])
t = 1
print("Plane Stress")
E = 1
nu = 0.3
Cc = E/(1+nu)/(1-nu)*Matrix([[1,nu,0],[nu,1,0],[0,0,(1-nu)/2]])
a = Matrix([coordinates[0,:], coordinates[4,:], coordinates[1,:], coordinates[5,:], coordinates[2,:], coordinates[6,:], coordinates[3,:], coordinates[7,:],coordinates[0,:]])
fig = plt.figure()
ax = fig.add_subplot(111)
cp = ax.plot(a[:,0], a[:,1])
ax.grid(True, which='both')
plt.xlabel("x")
plt.ylabel("y")
plt.title("Shape")
ax.axhline(y = 0, color = 'k')
ax.axvline(x = 0, color = 'k')
Shapefun = Matrix([-(1-eta)*(1-xi)*(1+xi+eta)/4,
                   -(1-eta)*(1+xi)*(1-xi+eta)/4,
                   -(1+eta)*(1+xi)*(1-xi-eta)/4,
                   -(1+eta)*(1-xi)*(1+xi-eta)/4,
                   (1-eta)*(1-xi)*(1+xi)/2,
                   (1+xi)*(1-eta)*(1+eta)/2,
                   (1+eta)*(1-xi)*(1+xi)/2,
                   (1-xi)*(1-eta)*(1+eta)/2])
Nn = Matrix([[0 for x in range(16)] for y in range(2)])
for i in range(8):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]
x = sum([Shapefun[i]*coordinates[i,0] for i in range(8)])
y = sum([Shapefun[i]*coordinates[i,1] for i in range(8)])
J = Matrix([[x.diff(xi), x.diff(eta)],[y.diff(xi),y.diff(eta)]])
JinvT = J.inv().transpose()
mat1 = Matrix([[1,0,0,0],[0,0,0,1],[0,1,1,0]])
mat2 = Matrix([[JinvT[0,0],JinvT[0,1],0,0], 
               [JinvT[1,0],JinvT[1,1],0,0], 
               [0,0,JinvT[0,0],JinvT[0,1]], 
               [0,0,JinvT[1,0],JinvT[1,1]]])
mat3 = Matrix([[0 for x in range(16)] for y in range(4)])
for i in range(8):
    mat3[0,2*i] = mat3[2,2*i+1] = sp.diff(Shapefun[i],xi)
    mat3[1,2*i] = mat3[3,2*i+1] = sp.diff(Shapefun[i],eta)
B = mat1*mat2*mat3
# This takes a long, long amount of time (The integration)
k1 = N(B.transpose()*Cc*B,3)
Ka = zeros(16)
for i in range(16):
    for j in range(16):
        integrand=k1[i,j]*J.det()
        intlam=lambdify((eta,xi),integrand)
        #mp.dps is for integral accuracy (# of decimal points)
        mp.dps = 3
        Ka[i,j] = quad(intlam,[-1,1],[-1,1])
print(k1)
rb = Matrix([rb1,rb2])
fbeforeintegration = Nn.transpose()*rb*J.det()
f3 = Matrix([integrate(fbeforeintegration[i],(xi,-1,1),(eta,-1,1))for i in range(16)])
print("Nodal Forces: ", f3)
