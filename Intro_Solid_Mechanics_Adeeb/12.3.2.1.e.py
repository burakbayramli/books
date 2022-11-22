import sympy as sp
from sympy import *
a, b, x1, x2, t1, t2, rb1, rb2 = symbols("a b x_1 x_2 t_1 t_2 rb_1 rb_2")
Shapefun = Matrix([(b-x2)*(a-x1)/4/a/b*-(1+x1/a+x2/b),
                   (b-x2)*(a+x1)/4/a/b*-(1-x1/a+x2/b),
                   (b+x2)*(a+x1)/4/a/b*-(1-x1/a-x2/b),
                   (b+x2)*(a-x1)/4/a/b*-(1+x1/a-x2/b),
                   (b-x2)*(a-x1)*(a+x1)/2/a**2/b,
                   (a+x1)*(b-x2)*(b+x2)/2/a/b**2,
                   (b+x2)*(a-x1)*(a+x1)/2/a**2/b,
                   (a-x1)*(b-x2)*(b+x2)/2/a/b**2])
Nn = zeros(2,16)
for i in range(8):
    Nn[0,2*i] = Nn[1,2*i+1] = Shapefun[i]
display("Nn:", Nn)
tn = Matrix([t1,t2])
rb = Matrix([rb1,rb2])
integrand1 = (Nn.transpose()*tn).subs(x1,-a)
integrand2 = (Nn.transpose()*rb)
fetraction = Matrix([simplify(integrate(integrand1[i],(x2,-b,b)))for i in range (16)])
febodyforces = Matrix([simplify(integrate(integrand2[i],(x1,-a,a),(x2,-b,b)))for i in range (16)])
display("fetraction: ", fetraction)
display("febodyforces: ", febodyforces)
