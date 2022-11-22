import sympy as sp
from sympy import *
a, b, x1, x2, nu, E, t = symbols("a b x_1 x_2 nu E t")
Shapefun = Matrix([(b-x2)*(a-x1)/4/a/b,(b-x2)*(a+x1)/4/a/b,(b+x2)*(a+x1)/4/a/b,(b+x2)*(a-x1)/4/a/b])
B = zeros(3,8)
for i in range(4):
    B[0,2*i] = B[2,2*i+1] = Shapefun[i].diff(x1)
    B[1,2*i+1] = B[2,2*i] = Shapefun[i].diff(x2)
display("B:", B)
Cc = E/(1 + nu)*Matrix([[(1-nu)/(1-2*nu),nu/(1-2*nu),0], 
                         [nu/(1-2*nu),(1-nu)/(1-2*nu), 0], 
                         [0, 0, 1/2]])  
KBeforeintegration = t * B.transpose()*Cc*B
K = Matrix([[simplify(integrate(KBeforeintegration[i,j],(x1,-a,a),(x2,-b,b)))for i in range (8)] for j in range (8)])
display("K: ", K)
