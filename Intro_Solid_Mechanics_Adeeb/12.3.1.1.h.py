import sympy as sp
from sympy import Matrix,integrate
x1,x2,t1,t2 = sp.symbols("x_1 x_2 t_1 t_2")
Shapefun = Matrix([[1-x1-x2],[x1],[x2]])
Nn = Matrix([[0 for x in range(6)] for y in range(2)])
for i in range(3):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]
tn = Matrix([[t1],[t2]])
f = Matrix([[(integrate(i,(x2,0,1))).subs({x1:0})]for i in Nn.T*tn])
display("Nodal Force due to Traction Vector on One Side:")
display("f^e =",f)
