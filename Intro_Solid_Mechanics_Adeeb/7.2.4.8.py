from sympy import *
import sympy as sp
sp.init_printing(use_latex = "mathjax")
def vonMises(M):
    return sp.sqrt(1/2*((M[0,0] - M[1,1])**2+(M[1,1] - M[2,2])**2+
                       (M[2,2] - M[0,0])**2+6*(M[0,1]**2+M[0,2]**2+M[1,2]**2)))

P, r, t, Ee, Nu =  3, 1000, 10, 210000, 0.3
s = Matrix([[P*r/2/t, 0, 0], 
            [0, P*r/t, 0],
            [0, 0, 0]])
print("stress matrix: ", s)
a = vonMises(s)
print("von Mises Stress: ", a)
G = Ee/2/(1+Nu)
Ss = Matrix([[1/Ee ,-Nu/Ee, -Nu/Ee, 0, 0, 0], 
             [-Nu/Ee, 1/Ee, -Nu/Ee, 0, 0, 0],
             [-Nu/Ee, -Nu/Ee, 1/Ee, 0, 0, 0],
             [0, 0, 0, 1/G, 0, 0],
             [0, 0, 0, 0, 1/G, 0],
             [0, 0, 0, 0, 0, 1/G]])
stressvector = Matrix([[s[0,0]], [s[1,1]], [s[2,2]], [s[0,1]], [s[0,2]], [s[1,2]]])
strainvector = Ss * stressvector
print("strainvector: ", strainvector)
deltadiameter = strainvector[1] * 2 * r
print("change in diameter: ", deltadiameter)
deltathickness = strainvector[2] * t
print("change in thickness: ", deltathickness)
