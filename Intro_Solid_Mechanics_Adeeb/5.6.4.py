from sympy import Matrix,N
import sympy as sp
sp.init_printing(use_latex="mathjax")
s1 = Matrix([[25,40,0],[40,20,30],[0,30,20]])
eVals = Matrix([i[0] for i in s1.eigenvects()]).evalf()
#Added chop=True to remove the small numbers.
display("eigenvalues (principal Stresses) =",N(eVals,chop=True))
e1, e2, e3 = eVals
maxShearStress = max([abs(e1-e2),abs(e2-e3),abs(e2-e3)])/2
display("max shear stress =",maxShearStress)
MisesStress = sp.sqrt((abs(e1-e2)**2+abs(e1-e3)**2+abs(e2-e3)**2)/2)
display("Von Mises stress =",MisesStress.evalf())
