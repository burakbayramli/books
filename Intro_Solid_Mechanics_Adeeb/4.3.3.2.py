from sympy import *
import sympy as sp
theta = sp.symbols("\u03B8")
X1,X2,X3=sp.symbols("X1 X2 X3")
x1,x2,x3=sp.symbols("x1 x2 x3")
Qx = Matrix([[1, 0, 0], 
              [0, sp.cos(theta),-sp.sin(theta)], 
              [0, sp.sin(theta),sp.cos(theta)]])
# y rotation matrix
Qy = Matrix([[sp.cos(theta), 0, sp.sin(theta)], 
              [0, 1, 0], 
              [-sp.sin(theta), 0, sp.cos(theta)]])
# z rotation matrix
Qz = Matrix([[sp.cos(theta), -sp.sin(theta), 0],
              [sp.sin(theta), sp.cos(theta), 0], 
              [0, 0, 1]])
Qx = Qx.subs({theta:30*sp.pi/180})
Qy = Qy.subs({theta:20*sp.pi/180})
Qz = Qz.subs({theta:20*sp.pi/180})
F = Qx*Qy*Qz
display("F =",F)
F = simplify(F).evalf()
display("F =",F)
X=Matrix([X1,X2,X3])
x=Matrix([x1,x2,x3])
display("X =",X)
display("x =",x)
display("x =F.X =",F*x)
x = F*X
u = x-X
display("u = x-X =",u)
gradu = Matrix([[diff(i,j) for j in X] for i in u])
display("\u2207u =",gradu)
small_strain = (gradu+gradu.T)/2
display("\u03B5_small =",small_strain)
green_strain = (gradu+gradu.T+gradu.T*gradu)/2
display("\u03B5_Green =",green_strain)
