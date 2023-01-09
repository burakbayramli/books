from sympy import *
import sympy as sp
sp.init_printing(use_latex="mathjax")
small_strain = Matrix([[0.01, 0.012],[0.012, 0]])
print("\u03B5_small =",small_strain) 
# eigenvalues
eigensystem = small_strain.eigenvects()
print("eigensystem", eigensystem)
eigenvalues = [i[0] for i in eigensystem]
eigenvectors = [i[2][0].T for i in eigensystem]
neigenvectors = [i/i.norm() for i in eigenvectors]
print("eigenvalues (principal strains) =",eigenvalues)
print("eigenvectors (principal directions) =",eigenvectors)
print("Normalized eigenvectors (principal directions) =",neigenvectors)
#The function Matrix ensures the list is a sympy matrix
Q = Matrix(neigenvectors)
print("Diagonalized matrix = Q.M.Q^T",Q*small_strain*Q.T)
print("Diagonalized matrix = Q.M.Q^T",N(Q*small_strain*Q.T,chop=True))
