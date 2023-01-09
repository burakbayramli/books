from sympy import *
import sympy as sp
sp.init_printing(use_latex = "mathjax")
Ee, Nu = 10000, 0.2
Cc = Matrix([[(1 - Nu)/(1 - 2*Nu), Nu/(1 - 2*Nu), 0], 
             [Nu/(1 - 2*Nu), (1 - Nu)/(1 - 2*Nu), 0],
             [0, 0, 1/2]])
scalar = Ee/(1+Nu) 
Cc = scalar * Cc
strvector  = Matrix([0.001, -0.002, 0.003])
stressvector = Cc * strvector
s = Matrix([[stressvector[0], stressvector[2], 0], 
            [stressvector[2], stressvector[1], 0],
            [0, 0, 0]])
s[2,2] = Nu*(s[0,0] + s[1,1])
print("stress matrix: ", s)
