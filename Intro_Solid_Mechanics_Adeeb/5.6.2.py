from sympy import Matrix, N
import sympy as sp
sp.init_printing(use_latex="mathjax")
s = Matrix([[2,2,0],[2,5,0],[0,0,-5]])
display("\u03C3 =",s)
eigensystem = s.eigenvects()
display("eigensystem", eigensystem)
eigenvalues = [i[0] for i in eigensystem]
eigenvectors = [i[2][0].T for i in eigensystem]
neigenvectors = [(i/i.norm()).evalf() for i in eigenvectors]
display("eigenvalues (principal Stresses) =",eigenvalues)
display("eigenvectors (principal directions) =",eigenvectors)
display("Normalized eigenvectors (principal directions) =",neigenvectors)
Q = Matrix(neigenvectors)
# rearrange 
Q = Matrix([Q[1,:],Q[0,:],Q[2,:]])
display("Q =",Q.evalf(3))
sp = N(Q*s*Q.T,chop=True)
display("\u03C3' =",sp)
