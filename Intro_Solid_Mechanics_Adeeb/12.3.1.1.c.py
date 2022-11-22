import sympy as sp
from sympy import Matrix,integrate
x1,x2,rb1,rb2 = sp.symbols("x_1 x_2 rho_1 rho_2")
Shapefun = Matrix([[1-x1-x2],[x1],[x2]])
Nn = Matrix([[0 for x in range(6)] for y in range(2)])
for i in range(3):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]
rb = Matrix([[rb1],[rb2]])
f = Matrix([[integrate(i,(x1,0,1-x2),(x2,0,1))]for i in Nn.T*rb])
display("Nodal Force due to Body Force:")
display("f^e =",f)
