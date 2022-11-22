import sympy as sp
from sympy import Matrix,simplify
Ee,nu,t = sp.symbols("E nu t")
B = Matrix([[-1,0,1,0,0,0],[0,-1,0,0,0,1],[-1,-1,0,1,1,0]])

Cc = Ee*t/2/(1 + nu)*Matrix([[(1-nu)/(1-2*nu),nu/(1-2*nu),0], 
                         [nu/(1-2*nu),(1-nu)/(1-2*nu), 0], 
                         [0, 0, 1/2]])
K = simplify(B.T*Cc*B)
display("K^e =",K)
