
# check convergence rate of fem, similar to 
# previous global func  


import sympy as sym
import numpy as np
from fe_approx1D_numint import *



Omega = [-1, 1]
symbolic = False
N = 10

vertices, cells, dof_map = mesh_uniform(N, 2, Omega, symbolic)  


x = sym.Symbol("x")
gauss_exact = sym.exp(-x**2) - sym.exp(-1)

u = approximate(gauss_exact, symbolic, 1, N, None, Omega) 

vertices = np.array(vertices)
gauss_bell = np.exp(-vertices**2)
gauss_bell -= gauss_bell[0]


import pylab 
pylab.plot(gauss_bell)
pylab.plot(u)
pylab.show()








