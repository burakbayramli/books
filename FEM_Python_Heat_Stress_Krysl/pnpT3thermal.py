# Finite Element Modeling with Abaqus and Python for Thermal and
# Stress Analysis
# (C)  2017, Petr Krysl
#
"""
Strain patterns calculated for a single triangle.
"""

from numpy import array, dot, linalg

##
xall = array([[-1, -1/2], [3, 2], [1, 2]]) #Coordinates of the nodes
conn = [1, 2, 3] # The definition of the element, listing its nodes

gradNpar = array([[-1, -1], [1, 0], [0, 1]])  #Gradients of the basis fncs wrt the param. coords
zconn = array(conn)-1
x = xall[zconn,:] # The coordinates  of the three nodes
J = dot(x.T, gradNpar) # Compute the Jacobian matrix
gradN = dot(gradNpar, linalg.inv(J))

def Bn(gradNn):
    """
    One-liner function to compute the nodal strain-displacement matrix.
    """
    return array([[gradNn[0], 0],
                  [0, gradNn[1]],
                  [gradNn[1], gradNn[0]]])

B1=Bn(gradN[0,:])
B2=Bn(gradN[1,:])
B3=Bn(gradN[2,:])

from sympy import symbols, Matrix, simplify

DeltaT, CTE, E, nu, t  = symbols('DeltaT CTE E nu t')
D = E/(1-nu**2)*Matrix([[1, nu, 0], [nu, 1, 0], [0, 0, (1-nu)/2]])
eth = DeltaT*CTE*Matrix([1, 1, 0]).reshape(3, 1)
sig = D*eth
Se = linalg.det(J)/2
F1 = simplify(Se*t*B1.T*sig)
F2 = simplify(Se*t*B2.T*sig)
F3 = simplify(Se*t*B3.T*sig)
print('Thermal forces')
print('Node 1:', F1)
print('Node 2:', F2)
print('Node 3:', F3)