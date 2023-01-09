import sympy as sp
from sympy import *
a, b, x1, x2, E, t, nu = symbols("a b x_1 x_2 E t nu")
Shapefun = Matrix([(b-x2)*(a-x1)/4/a/b*-(1+x1/a+x2/b),
                   (b-x2)*(a+x1)/4/a/b*-(1-x1/a+x2/b),
                   (b+x2)*(a+x1)/4/a/b*-(1-x1/a-x2/b),
                   (b+x2)*(a-x1)/4/a/b*-(1+x1/a-x2/b),
                   (b-x2)*(a-x1)*(a+x1)/2/a**2/b,
                   (a+x1)*(b-x2)*(b+x2)/2/a/b**2,
                   (b+x2)*(a-x1)*(a+x1)/2/a**2/b,
                   (a-x1)*(b-x2)*(b+x2)/2/a/b**2])
B = zeros(3,16)
for i in range(8):
    B[0,2*i] = B[2,2*i+1] = Shapefun[i].diff(x1)
    B[1,2*i+1] = B[2,2*i] = Shapefun[i].diff(x2)
print("B:", B)
Cc = E/(1 + nu)*Matrix([[(1-nu)/(1-2*nu),nu/(1-2*nu),0], 
                         [nu/(1-2*nu),(1-nu)/(1-2*nu), 0], 
                         [0, 0, 1/2]])  
KBeforeintegration = t * B.transpose()*Cc*B
K = Matrix([[simplify(integrate(KBeforeintegration[i,j],(x1,-a,a),(x2,-b,b)))for i in range (16)] for j in range (16)])
print("K: ", K)
