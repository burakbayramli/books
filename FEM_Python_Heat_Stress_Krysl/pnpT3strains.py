# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
# Strain patterns calculated for a single triangle.

from numpy import *

## 
xall= array([[-1, -1/2], [3, 2], [1, 2]]) #Coordinates of the nodes
conn= [1,2,3] # The definition of the element, listing its nodes

gradNpar= array([[-1,-1], [1,0], [0,1]])  #Gradients of the basis fncs wrt the param. coords
zconn=array(conn)-1
x=xall[zconn,:] # The coordinates  of the three nodes
J=dot(x.T, gradNpar) # Compute the Jacobian matrix
gradN=dot(gradNpar, linalg.inv(J))

## 
# Anonymous function to calculate one nodal strain-displacement matrix
def Bn(gradNn):
    return array([[gradNn[0], 0],
                  [0, gradNn[1]],
                  [gradNn[1], gradNn[0]]]) 

## 
# 
B1=Bn(gradN[0,:])
B2=Bn(gradN[1,:])
B3=Bn(gradN[2,:])

from sympy import *

psi, Xc, Yc =symbols('psi Xc Yc')
x1, y1, x2, y2, x3, y3 =symbols('x1 y1 x2 y2 x3 y3')  
def rot(X):
    return Matrix([[0,-psi], [psi,0]])*(Matrix(X)-Matrix([[Xc], [Yc]])) 

u1=rot([x1, y1])
u2=rot([x2, y2])
u3=rot([x3, y3])
e=B1*u1+B2*u2+B3*u3
print(simplify(e))

print(simplify(e.subs(x1,xall[0,0]).subs(x2,xall[1,0]).subs(x3,xall[2,0])\
               .subs(y1,xall[0,1]).subs(y2,xall[1,1]).subs(y3,xall[2,1])\
                    ))