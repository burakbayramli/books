from sympy import Matrix
import sympy as sp
sp.init_printing(use_latex="mathjax")
s = Matrix([[1,-1,0],[-1,-5,0],[0,0,4]])
display("\u03C3 =",s)
u = Matrix([1,1,1])
display("u = ",u)
n = u/u.norm()
display("n = ",n)
t = s*n
display("traction vector = \u03C3n =",t)
n_stress = t.dot(n)
display("normal stress =", n_stress)
shear_stress = t - n_stress*n
display("shear stress vector on n =",shear_stress)
display("\u03C4_n =", shear_stress.norm())
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
Q = Matrix([Q[2,:],Q[1,:],Q[0,:]])
display("Q =",Q.evalf(3))
sp = Q*s*Q.T
display("\u03C3' = Q*\u03C3*Q^T =",sp.evalf(3))
