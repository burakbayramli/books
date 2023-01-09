from sympy import Matrix, N
import sympy as sp
sp.init_printing(use_latex="mathjax")
s = Matrix([[2,2,0],[2,5,0],[0,0,-5]])
print("\u03C3 =",s)
eigensystem = s.eigenvects()
print("eigensystem", eigensystem)
eigenvalues = [i[0] for i in eigensystem]
eigenvectors = [i[2][0].T for i in eigensystem]
neigenvectors = [(i/i.norm()).evalf() for i in eigenvectors]
print("eigenvalues (principal Stresses) =",eigenvalues)
print("eigenvectors (principal directions) =",eigenvectors)
print("Normalized eigenvectors (principal directions) =",neigenvectors)
Q = Matrix(neigenvectors)
# rearrange 
Q = Matrix([Q[1,:],Q[0,:],Q[2,:]])
print("Q =",Q.evalf(3))
sp = N(Q*s*Q.T,chop=True)
print("\u03C3' =",sp)
