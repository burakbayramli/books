from sympy import *
x = Symbol('x')
print cos(acos(x))
dcos = diff(cos(2*x), x)
dcos
print dcos
print dcos.subs(x, pi).evalf()  # x=pi, float evaluation
S = sin(x).series(x, 4)
print S
I = integrate(log(x), x)
print I
