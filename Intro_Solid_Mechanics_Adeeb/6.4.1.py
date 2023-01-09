from sympy import Matrix,diff,symbols
import sympy as sp
sp.init_printing(use_latex="mathjax")
x1,x2,x3 = symbols("x1 x2 x3")
s = Matrix([[5*x1**2+3*x2+x3, -x2*x1, 0],[-x2*x1,5*x2,0],[0,0,9*x3**2]])
print("stress field over a body")
print("\u03C3 =", s)
x = Matrix([x1,x2,x3])
pb = Matrix([-sum([diff(s[i,j], x[i]) for i in range(3)]) for j in range(3)])
print("vector field")
print("pb =",pb) 
