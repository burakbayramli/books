from sympy import Matrix
import sympy as sp
sp.init_printing(use_latex="mathjax")
s = Matrix([[1,-1,0],[-1,-5,0],[0,0,4]])
print("\u03C3 =",s)
u = Matrix([1,1,1])
print("u = ",u)
n = u/u.norm()
print("n = ",n)
t = s*n
print("traction vector = \u03C3n =",t)
n_stress = t.dot(n)
print("normal stress =", n_stress)
shear_stress = t - n_stress*n
print("shear stress vector on n =",shear_stress)
print("\u03C4_n =", shear_stress.norm())
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
Q = Matrix([Q[2,:],Q[1,:],Q[0,:]])
print("Q =",Q.evalf(3))
sp = Q*s*Q.T
print("\u03C3' = Q*\u03C3*Q^T =",sp.evalf(3))
