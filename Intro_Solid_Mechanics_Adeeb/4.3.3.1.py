from sympy import *
import sympy as sp
sp.init_printing(use_latex="mathjax")
F = Matrix([[1.2,0.2,0.2],[0.2,1.3,0.1],[0.9,0.5,1]])
X1,X2,X3 = sp.symbols("X1 X2 X3")
x1,x2,x3 = sp.symbols("x1 x2 x3")
X = Matrix([X1,X2,X3])
x = Matrix([x1,x2,x3])
print("X =",X)
print("x =",x)
print("x = F.X =",F*x)
x = F*X
u = x-X
print("u = x-X =",u)
gradu = Matrix([[diff(i,j) for j in X] for i in u])
print("\u2207u =",gradu)
small_strain = (gradu+gradu.T)/2
print("\u03B5_small =",small_strain)
green_strain = (gradu+gradu.T+gradu.T*gradu)/2
print("\u03B5_Green =",green_strain)
v = Matrix([1,1,1])
n = v/v.norm()
print("small strain along u", n.dot(small_strain*n))
print("Green strain along u", n.dot(green_strain*n))
