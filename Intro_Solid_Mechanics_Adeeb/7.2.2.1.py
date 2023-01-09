from sympy import Matrix,fraction
from sympy.solvers.inequalities import reduce_rational_inequalities
import sympy as sp
Ee,Nu = sp.symbols("E nu", real = True)
G = Ee/2/(1 + Nu)
Cc = Matrix([[1/Ee,-Nu/Ee,-Nu/Ee,0,0,0],
              [-Nu/Ee,1/Ee, -Nu/Ee,0,0,0], 
              [-Nu/Ee, -Nu/Ee, 1/Ee,0, 0, 0], 
              [0, 0, 0, 1/G, 0, 0], 
              [0, 0, 0, 0, 1/G, 0], 
              [0, 0, 0, 0, 0, 1/G]])
print("C =",Cc)
eigensystem = Cc.eigenvects()
eigenvalues = [i[0] for i in eigensystem]
print("eigenvalues =",eigenvalues)
# I have not found an algorithm where sympy can solve multiple values in inequalities
n1,d1 = fraction(eigensystem[0][0])
n2,d2 = fraction(eigensystem[1][0])
n3,d3 = fraction(eigensystem[2][0])
s_Nu = reduce_rational_inequalities([[n1>0,n2>0,n3>0]], Nu)
s_Ee = reduce_rational_inequalities([[1/d1>0,1/d2>0,1/d3>0]], Ee)
print("Inequalities for Eigenvalues to be positive:",s_Nu,s_Ee)
