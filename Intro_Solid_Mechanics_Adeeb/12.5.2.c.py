import sympy as sp
from sympy import Matrix,diff
xi,eta = sp.symbols("xi eta")
nodes = [[0,0],[5,0],[2,6],[0,6]]
el1 = Matrix([1,2,3])
el2 = Matrix([1,3,4])
e1 = Matrix([nodes[0],nodes[1],nodes[2]])
e2 = Matrix([nodes[0],nodes[2],nodes[3]])
map1 = [2*el1[0]-1,2*el1[0],2*el1[1]-1,2*el1[1],2*el1[2]-1,2*el1[2]]
map2 = [2*el2[0]-1,2*el2[0],2*el2[1]-1,2*el2[1],2*el2[2]-1,2*el2[2]]
E = 20000000000
nu = 0.2
Nn = Matrix([[1-xi-eta],[xi],[eta]])
Cc1 = E/(1+nu)*Matrix([[(1-nu)/(1-2*nu),nu/(1-2*nu),0], 
                       [nu/(1-2*nu),(1-nu)/(1-2*nu),0], 
                       [0,0,1/2]])
display("Plane Strain: ", Cc1)
def calculate(e):
    xe = sum(Nn[i]*e[i,0] for i in range(3)) 
    ye = sum(Nn[i]*e[i,1] for i in range(3))
    display("Mapping functions =",xe,ye)
    J = Matrix([[xe.diff(xi), xe.diff(eta)], [ye.diff(xi), ye.diff(eta)]])
    display("Jacobian Transformation =", J)
    Jin1t = J.T.inv()
    display("Inverse =", Jin1t)
    JD = J.det()
    display("Determinant =", JD)
    Mat1 = Matrix([[1,0,0,0],[0,0,0,1],[0,1,1,0]])
    Mat2 = Matrix([[Jin1t[0,0], Jin1t[0,1],0,0],
                    [Jin1t[1,0],Jin1t[1,1],0,0],
                    [0,0,Jin1t[0,0],Jin1t[0,1]],
                    [0,0,Jin1t[1,0],Jin1t[1,1]]])
    Mat3 = Matrix([[Nn[0].diff(xi),0,Nn[1].diff(xi),0,Nn[2].diff(xi),0],
                   [Nn[0].diff(eta),0,Nn[1].diff(eta),0,Nn[2].diff(eta),0],
                   [0,Nn[0].diff(xi),0,Nn[1].diff(xi),0,Nn[2].diff(xi)],
                   [0,Nn[0].diff(eta),0,Nn[1].diff(eta),0,Nn[2].diff(eta)]])
    B = Mat1*Mat2*Mat3
    display("B =", B)
    Kb2 = B.T*Cc1*B
    K = JD*Kb2
    Ki = (1/2)*K
    display("Ki =",Ki)
    return Ki
display("Element E1")
Ki1 = calculate(e1)
display("Element E2")
Ki2 = calculate(e2)
ktotal = Matrix([[0 for x in range(8)] for y in range(8)])
for i in range(6):
    for j in range(6):
        i1,j1 = map1[i]-1,map1[j]-1
        i2,j2 = map2[i]-1,map2[j]-1
        ktotal[i1,j1] = ktotal[i1,j1]+Ki1[i,j]
        ktotal[i2,j2] = ktotal[i2,j2]+Ki2[i,j]
display("ktotal =",ktotal)
Kk = Matrix(ktotal[4:8,4:8])
display(Kk)
ff = Matrix([0,0,0,-50000000])
display("Force vector =",ff)
u = Kk.inv()*ff
display("Displacements =",u)
