import sympy as sp
from sympy import simplify,diff,integrate
x1,x2,t,Ee,nu = sp.symbols("x_1 x_2 t Ee nu")
half=sp.sympify('1/2') 
Shapefun = Matrix([[2*(1-x1-x2)*(half-x1-x2)],[2*x1*(x1-half)],
                   [2*x2*(x2-1/2)],[4*x1*(1-x1-x2)],
                   [4*x1*x2],[4*x2*(1-x1-x2)]])
B = Matrix([[0 for x in range(12)] for y in range(3)])
for i in range(6):
    B[0,2*i] = B[2,2*i+1] = diff(Shapefun[i],x1)
    B[1,2*i+1] = B[2,2*i] = diff(Shapefun[i],x2)                                                                                                   
Cc = Ee/(1 + nu)*Matrix([[(1-nu)/(1-2*nu),nu/(1-2*nu),0], 
                         [nu/(1-2*nu),(1-nu)/(1-2*nu), 0], 
                         [0, 0, 1/2]])   
Kbeforeintegration = t*simplify(B.T*Cc*B)
K = Matrix([[simplify(integrate(Kbeforeintegration[i,j],(x1,0,1-x2),(x2,0,1)))for i in range (12)] for j in range (12)])
display("K^e =",K)
