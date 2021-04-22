# Finite Element Modeling with Abaqus and Matlab for  Thermal and 
# Stress Analysis
# (C)  2015, Petr Krysl
#
# Concrete column with hydration heat and zero temperature on the boundary.
# Numerical solution.

from numpy import *
from math import * 

## 
# These are the constants in the problem, k is kappa
a=2.5
dy=a/2*sin(15./180*pi)
dx=a/2*cos(15./180*pi)
Q=4.5
k=1.8
Dz=1.0

#  Gradients of the basis functions wrt the parametric coords
gradNpar= array([[-1,-1],[1,0],[0,1]]) 
#Coordinates of the nodes. Node 1 in first row, and so on.
xall= array([[0,0], [dx,-dy], [dx,dy], [2*dx,-2*dy], [2*dx,2*dy]])
# Numbers of the degrees of freedom
dof=array([3, 1, 2, 5, 4])
# Number of free degrees of freedom
N_f=3

# 
# Global conductivity matrix and heat load vector.
K=zeros(25).reshape(5,5)
L=zeros(5).reshape(5,1)
# 
 
#First element
conn= [1,2,3] # The definition of the element, listing its nodes
zconn=array(conn)-1 # zero-based node indexes
x=xall[zconn,:]# The coordinates  of the three nodes
print('x=',x)
J=dot(x.T, gradNpar) # Compute the Jacobian matrix
Se=linalg.det(J)/2 # The area of the triangle
print('Se=',Se)
# Compute the gradient with respect to X, Y
gradN=dot(gradNpar, linalg.inv(J))
print('gradN=',gradN)
#
## Some terms of the conductivity matrix
print(Se*dot(gradN[0,:], gradN[0,:].T)*k*Dz)
print(Se*dot(gradN[0,:], gradN[1,:].T)*k*Dz)
# The entire elementwise conductivity matrix
Ke1= (Se*dot(gradN, gradN.T)*k*Dz)
print('Ke1=',Ke1)
# Element degree-of-freedom array,  converted to zero base
zedof=array(dof[zconn])-1
#The indexing grid into the global conductivity matrix
ixgrid =ix_(zedof,zedof)
# Assemble contribution from element 1
K[ixgrid]=K[ixgrid]+Ke1
print(K)
# Compute heat load from element 1
LQe1=Se*Q*Dz/3*ones(3).reshape(3,1)
L[zedof]=L[zedof]+LQe1
 
##Second element
conn= [2,4,5]
zconn=array(conn)-1 # zero-based node indexes
x=xall[zconn,:]# The coordinates  of the three nodes
J=dot(x.T, gradNpar) # Compute the Jacobian matrix
Se=linalg.det(J)/2 # The area of the triangle
# Compute the gradient with respect to X, Y
gradN=dot(gradNpar, linalg.inv(J))
# The entire elementwise conductivity matrix
Ke2= (Se*dot(gradN, gradN.T)*k*Dz)
print(Ke2)
# Element degree-of-freedom array,  converted to zero base
zedof=array(dof[zconn])-1
#The indexing grid into the global conductivity matrix
ixgrid =ix_(zedof,zedof)
# Assemble contribution from element 1
K[ixgrid]=K[ixgrid]+Ke2
print(K)
# Compute heat load from element 1
LQe2=Se*Q*Dz/3*ones(3).reshape(3,1)
L[zedof]=L[zedof]+LQe2
 
##Third element
conn= [2,5,3]
zconn=array(conn)-1 # zero-based node indexes
x=xall[zconn,:]# The coordinates  of the three nodes
J=dot(x.T, gradNpar) # Compute the Jacobian matrix
Se=linalg.det(J)/2 # The area of the triangle
# Compute the gradient with respect to X, Y
gradN=dot(gradNpar, linalg.inv(J))
# The entire elementwise conductivity matrix
Ke3= (Se*dot(gradN, gradN.T)*k*Dz)
print(Ke3)
# Element degree-of-freedom array,  converted to zero base
zedof=array(dof[zconn])-1
#The indexing grid into the global conductivity matrix
ixgrid =ix_(zedof,zedof)
# Assemble contribution from element 1
K[ixgrid]=K[ixgrid]+Ke3
print(K)
# Compute heat load from element 1
LQe3=Se*Q*Dz/3*ones(3).reshape(3,1)
L[zedof]=L[zedof]+LQe3


## 
# Solution:
K=K[0:N_f,0:N_f]
L=L[0:N_f]
T=linalg.solve(K,L) 
print(T)
#
#
#
