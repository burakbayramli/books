from sympy import *
import sympy as sp
sp.init_printing(use_latex="mathjax")
X1, X2, X3 = sp.symbols("X_1 X_2 X_3")
theta = sp.symbols("\u03B8")
X = Matrix([X1, X2, X3])
F = Matrix([[1,tan(theta),0],[0,1,0],[0,0,1]])
print("deformation gradient:")
print("F =",F)
x = F*X
print("position function:")
print("x =",x)
u = x-X
print("u = x-X =",u)
gradu = Matrix([[diff(i,j) for j in X] for i in u])
print("\u2207u =",gradu)
small_strain = (gradu+gradu.T)/2
print("\u03B5_small =",small_strain)
green_strain = (gradu+gradu.T+gradu.T*gradu)/2
print("\u03B5_Green =",green_strain)
