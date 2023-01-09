from sympy import *
import sympy as sp
xi,eta = sp.symbols("xi eta")
nodes = [[0,0], [5,0], [2,6]]
el1 = Matrix([1,2,3])
e1 = Matrix([[0,0], [5,0], [2,6]])
E = 20000000000
nu = 0.2
Nn = Matrix([[1-xi-eta], [xi], [eta]])
print("Shape Functions: ", Nn)
Cc2 = E/(1+nu)*Matrix([[1/(1 - nu), nu/(1 - nu), 0], 
                       [nu/(1 - nu), 1/(1 - nu), 0], 
                       [0, 0, 1/2]])
print("Plane Stress: ", Cc2)
Cc1 = E/(1+nu)*Matrix([[(1 - nu)/(1 - 2 * nu), nu/(1 - 2 * nu), 0], 
                       [nu/(1 - 2 * nu), (1 - nu)/(1 - 2 * nu), 0], 
                       [0, 0, 1/2]])
print("Plane Strain: ", Cc1)
xe1 = sum(Nn[i] * e1[i,0] for i in range(3)) 
ye1 = sum(Nn[i] * e1[i,1] for i in range(3)) 
print("Mapping functions: ",xe1,ye1)
J1 = Matrix([[xe1.diff(xi), xe1.diff(eta)], [ye1.diff(xi), ye1.diff(eta)]])
print("Jacobian Transformation: ", J1)
lol = Inverse(J1)
J1in1t = J1.inv()
print("Inverse: ", J1in1t)
JD1 = J1.det()
print("Determinant: ", JD1)
Mat1 = Matrix([[1,0,0,0],[0,0,0,1],[0,1,1,0]])
Mat2 = Matrix([[J1in1t[0,0], J1in1t[1,0], 0, 0],
                 [J1in1t[0,1], J1in1t[1,1], 0, 0],
                 [0, 0, J1in1t[0,0], J1in1t[1,0]],
                 [0, 0, J1in1t[0,1], J1in1t[1,1]]])
Mat3 = Matrix([[Nn[0].diff(xi), 0, Nn[1].diff(xi), 0, Nn[2].diff(xi), 0],
               [Nn[0].diff(eta), 0, Nn[1].diff(eta), 0, Nn[2].diff(eta), 0],
               [0, Nn[0].diff(xi), 0, Nn[1].diff(xi), 0, Nn[2].diff(xi)],
               [0, Nn[0].diff(eta), 0, Nn[1].diff(eta), 0, Nn[2].diff(eta)]])
B = Mat1*Mat2*Mat3
print("B: ", B)
Kb2 = B.transpose()*Cc1*B
K1 = JD1*Kb2
Ki1 = sympify(1/2)*K1
K2 = JD1*B.transpose()*Cc2*B
Ki2 = 1/2*K2
K = Matrix([[Ki1[4,4], Ki1[4,5]],[Ki1[5,4], Ki1[5,5]]])
F = Matrix([50000000, -50000000])
u = K.inv()*F
ud = Matrix([0,0,0,0,u[0],u[1]])
print("strain: ", B*ud)
