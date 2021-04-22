# Finite Element Modeling with Abaqus and Python for Thermal and 
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
 A single quadrilateral element of rectangular shape.  Elementwise 
 conductivity matrix. Evaluated symbolically with numerical quadrature.
 """

from sympy import symbols, simplify, Matrix, det

# Define the basis function matrix
xi, eta = symbols('xi eta')
N = Matrix([(xi-1)*(eta-1)/4, 
            (xi+1)*(eta-1)/-4, 
            (xi+1)*(eta+1)/4,
            (xi-1)*(eta+1)/-4]).reshape(4,1)
print(N)

#Define geometry
A, B = symbols('A, B')
x = Matrix([[A, B], [0, B], [0, 0], [A, 0]])

#  Differentiate to obtain the Basis function gradients
gradNpar = diff(N, 'xi').row_join(diff(N, 'eta'))
print(gradNpar)

# Compute the Jacobian matrix
J = x.T*gradNpar
print(simplify(J))
#  The Jacobian
Jac = det(J)
print(Jac)

#  The  gradient  of the basis functions with respect to x,y is a
#  symbolic expression which needs to be evaluated for particular values of
#  xi,eta
gradN = simplify(gradNpar*(J**-1))
print(gradN)

# We introduce the the thermal
# conductivity  and the thickness of the slice, as symbolic variables.
kap, Dz = symbols('kap, Dz')

# Note that using subs() will substitute the values of the parametric
# coordinates.  The Jacobian and the material properties and the thickness 
# of the slice are assumed constant and hence are not included in this sum. 
# The weights of the for quadrature points are all 1.0.
# The formula (4.13) will be recognizable in the following lines.
gN = gradN.subs(xi, -0.577350269189626).subs(eta, -0.577350269189626)
K1 = kap*Dz*Jac*gN*gN.T # term 1 of the sum
gN = gradN.subs(xi, -0.577350269189626).subs(eta, +0.577350269189626)
K2 = kap*Dz*Jac*gN*gN.T # term 2 of the sum
gN = gradN.subs(xi, +0.577350269189626).subs(eta, -0.577350269189626)
K3 = kap*Dz*Jac*gN*gN.T # term 3 of the sum
gN = gradN.subs(xi, +0.577350269189626).subs(eta, +0.577350269189626)
K4 = kap*Dz*Jac*gN*gN.T # term 4 of the sum

K = simplify(K1+K2+K3+K4)
print(K)

print(K.eigenvals())