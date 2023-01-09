from sympy import *
import sympy as sp
sp.init_printing(use_latex = "mathjax")
E11, E22, E33, G12, G13, G23, nu31, nu32, nu21 = 100, 200, 350, 50, 70, 90, 0.2, 0.1, 0.05
nu13 = nu31/E33*E11
nu12 = nu21/E22*E11
nu23 = nu32/E33*E22
Ss = Matrix([[1/E11, -nu21/E22, -nu31/E33, 0, 0, 0],
             [-nu12/E11, 1/E22, -nu32/E33, 0, 0, 0],
             [-nu13/E11, -nu23/E22, 1/E22, 0, 0, 0],
             [0, 0, 0, 1/G12, 0, 0],
             [0, 0, 0, 0, 1/G13, 0],
             [0, 0, 0, 0, 0, 1/G23]])
s = Matrix([[1,0,0], [0,0,0], [0,0,0]])
print("stress matrix in B: ", s)
#this rotates clockwise, so use - of the angle
Q = rot_axis3(pi/4)
print("rotation matrix required: ", Q)
sdash = Q*s*Transpose(Q)
print("stress matrix in B': ", sdash)
sigmavector = Matrix([[sdash[0,0]], [sdash[1,1]], [sdash[2,2]], [sdash[0,1]], [sdash[0,2]], [sdash[1,2]]])
strvector = Ss*sigmavector
strdash = Matrix([[strvector[0], strvector[3]/2, strvector[4]/2], 
                  [strvector[3]/2, strvector[1], strvector[4]/2], 
                  [strvector[4]/2, strvector[5]/2, strvector[2]]])
print("strain matrix in  B': ", strdash)
strain = Transpose(Q) * strdash * Q
print("strain matrix in B: ", strain)
