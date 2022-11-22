from sympy import *
import sympy as sp
sp.init_printing(use_latex="mathjax")
small_strain = Matrix([[0.01, 0.012],[0.012, 0]])
display("\u03B5_small =",small_strain) 
# eigenvalues
eigensystem = small_strain.eigenvects()
display("eigensystem", eigensystem)
eigenvalues = [i[0] for i in eigensystem]
eigenvectors = [i[2][0].T for i in eigensystem]
neigenvectors = [i/i.norm() for i in eigenvectors]
display("eigenvalues (principal strains) =",eigenvalues)
display("eigenvectors (principal directions) =",eigenvectors)
display("Normalized eigenvectors (principal directions) =",neigenvectors)
#The function Matrix ensures the list is a sympy matrix
Q = Matrix(neigenvectors)
display("Diagonalized matrix = Q.M.Q^T",Q*small_strain*Q.T)
display("Diagonalized matrix = Q.M.Q^T",N(Q*small_strain*Q.T,chop=True))
