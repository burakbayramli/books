from sympy import *
import sympy as sp
sp.init_printing(use_latex="mathjax")
E11, E12, E22 = sp.symbols("\u03B5_{11} \u03B5_{12} \u03B5_{22}")
E_s = sp.symbols("\u03B5_{small}=")
E_small = Matrix([[E11, E12],[E12, E22]])
display(E_s,E_small)
theta = sp.symbols("\u03B8")
a = Matrix([sp.cos(theta), sp.sin(theta)])
b = Matrix([0,1])
c = Matrix([-sp.cos(theta), sp.sin(theta)])
a, c = a.subs({theta:30*sp.pi/180}), c.subs({theta:30*sp.pi/180})
display("a =",a)
display("b =",b)
display("c =",c)
strain = Matrix([Rational('5/1000'), Rational('2/1000'),Rational('-1/1000')])
eq1 = simplify(a.dot(E_small*a)) - strain[0]
eq2 = simplify(b.dot(E_small*b)) - strain[1]
eq3 = simplify(c.dot(E_small*c)) - strain[2]
display("Systems of equations:")
display(eq1)
display(eq2)
display(eq3)
sol = solve([eq1, eq2, eq3],[E11, E12, E22])
E_small = E_small.subs({E11:sol[E11],E12:sol[E12],E22:sol[E22]})
display(E_s,E_small)
E_small = N(E_small)
display(E_s,E_small)
eigensystem = E_small.eigenvects()
display("eigensystem", eigensystem)
eigenvalues=[i[0] for i in eigensystem]
eigenvectors=[i[2][0].T for i in eigensystem]
neigenvectors=[i/i.norm() for i in eigenvectors]
display("eigenvalues (principal strains) =",eigenvalues)
display("eigenvectors (principal directions) =",eigenvectors)
display("Normalized eigenvectors (principal directions) =",neigenvectors)
