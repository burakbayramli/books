import sympy as sp
from sympy import *
a, b, x1, x2, t1, t2, rb1, rb2 = symbols("a b x_1 x_2 t_1 t_2 rb_1 rb_2")
Shapefun = Matrix([(b-x2)*(a-x1)/4/a/b,(b-x2)*(a+x1)/4/a/b,(b+x2)*(a+x1)/4/a/b,(b+x2)*(a-x1)/4/a/b])
Nn= zeros(2,8)
for i in range(4):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]
print("Nn:", Nn)
tn = Matrix([t1,t2])
rb = Matrix([rb1,rb2])
integrand1 = (Nn.transpose()*tn).subs(x1,-a)
integrand2 = (Nn.transpose()*rb)
fetraction = Matrix([simplify(integrate(integrand1[i],(x2,-b,b)))for i in range (8)])
febodyforces = Matrix([simplify(integrate(integrand2[i],(x1,-a,a),(x2,-b,b)))for i in range (8)])
print("fetraction: ", fetraction)
print("febodyforces: ", febodyforces)
