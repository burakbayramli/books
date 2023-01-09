import sympy as sp
from sympy import simplify,integrate,N,Matrix
x1,x2,rb1,rb2 = sp.symbols("x_1 x_2 rho_1 rho_2")
half=sp.sympify('1/2') 
Shapefun = Matrix([[2*(1-x1-x2)*(half-x1-x2)],[2*x1*(x1-half)],
                   [2*x2*(x2-1/2)],[4*x1*(1-x1-x2)],
                   [4*x1*x2],[4*x2*(1-x1-x2)]])
Nn = Matrix([[0 for x in range(12)] for y in range(2)])
for i in range(6):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]                                                                                                 
rb = Matrix([rb1,rb2])
solve = Nn.T*rb
f = Matrix([[simplify(integrate(solve[i],(x1,0,1-x2),(x2,0,1)))]for i in range (12)])
print("Nodal Force due to Body Force:")
print("f^e =",f)
